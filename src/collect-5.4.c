#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <assert.h>
#include "scheme.h"

typedef struct root_t{
  int count;
  char** start;
  struct root_t* next;
} root_t;

void S_add_roots(pcb_t* pcb, int* f){
  int n = *f;
  if(n == 0) return;
  root_t* t = malloc(sizeof(root_t));
  if(t == NULL){
    fprintf(stderr, "Error mallocing\n");
    exit(-1);
  }
  t->count = n;
  t->start = (char**)(f+1);
  t->next = (root_t*) pcb->roots;
  pcb->roots = (char*) t;
  int i;
  for(i=1; i<=n; i++){
    assert(f[i] == 0);
  }
}

void S_check_roots(pcb_t* pcb, int* f){
  int n = *f;
  int i;
  for(i=1; i<=n; i++){
    assert(f[i] != 0);
  }
}

/* S_collect is called from scheme under the following conditions:
 * 1. An attempt is made to allocate a small object and the ap is above
 *    the red line.
 * 2. The current frame of the call is dead, so, upon return from S_collect,
 *    the caller returns to its caller.
 * 3. The frame-pointer of the caller to S_collect is saved at
 *    pcb->stack_extent.  No variables are live at that frame except for
 *    the return point (at *(pcb->stack_extent)).
 * 4. S_collect must return a new ap (in pcb->allocation_pointer) that has
 *    at least 2 pages of memory free.
 * 5. S_collect must also update pcb->allocaton_red_line to be 2 pages below
 *    the real end of heap.
 * 6. S_collect should not move the stack.
 */

#define pagesize 4096
#define minimum_heap_size (pagesize * 1024 * 4)
#define maximum_heap_size (pagesize * 1024 * 8)
#define minimum_stack_size (pagesize * 128)
#define align_to_page(x) (((x)/pagesize)*pagesize)
static char* allocate_unprotected_space(int size);
static void deallocate_unprotected_space(char* p, int size);
static void deallocate_string_pages(char*);
static void copy_roots(pcb_t* pcb);
static char* move_object(char* x, pcb_t* pcb);

pcb_t* S_collect(int req, pcb_t* pcb){
#if 0
  fprintf(stderr, "S_collect entry %d (pcb=0x%08x)\n", req, (int)pcb);
#endif
  char* heap_base = pcb->heap_base;
#if 0
  int heap_size = (int)pcb->heap_size;
  fprintf(stderr, "heapsize=0x%08x (0x%08x .. 0x%08x)\n", 
      heap_size,
      (int) heap_base,
      (int) (heap_base + heap_size - 1));
#endif
  int used_space = (int)(pcb->allocation_pointer - heap_base);
  {
    int bytes = (int) pcb->allocated_bytes + (used_space & 0xFFFFF);
    pcb->allocated_megs += (bytes >> 20);
    pcb->allocated_bytes = (char*) (bytes & 0xFFFFF);
#if 0
    fprintf(stderr, "allocated %d megs and %d bytes so far\n", 
        (int) pcb->allocated_megs, 
        (int) pcb->allocated_bytes);
#endif
  }

  int required_space = align_to_page(used_space + 2 * req + 2 * pagesize);
  if(required_space < minimum_heap_size){
    required_space = minimum_heap_size;
  }
  if(required_space > maximum_heap_size){
    fprintf(stderr, "Maximum heapsize exceeded\n");
    exit(-1);
  }
  char* old_heap = pcb->heap_base;
  int old_size = (int)pcb->heap_size;
  char* old_string_pages = pcb->string_pages;
  pcb->string_pages = 0;
  char* new_heap = allocate_unprotected_space(maximum_heap_size);
  pcb->allocation_pointer = new_heap;
  pcb->allocation_redline = new_heap + maximum_heap_size - 2 * pagesize;
  pcb->heap_base = new_heap;
  pcb->heap_size = (char*) maximum_heap_size;
  copy_roots(pcb);
  char** p = (char**) new_heap;
  while(p != (char**) pcb->allocation_pointer){
    *p = move_object(*p, pcb);
    p++;
  }
  deallocate_unprotected_space(old_heap, old_size);
  deallocate_string_pages(old_string_pages);
  {
    int free_space = 
      (int)pcb->allocation_redline - (int)pcb->allocation_pointer;
    int diff = align_to_page(free_space - minimum_heap_size);
    if(diff > 0){
      deallocate_unprotected_space(
          pcb->heap_base + (int)pcb->heap_size - diff,
          diff);
      pcb->allocation_redline -= diff;
      pcb->heap_size -= diff;
    }
  }
#if 0
  fprintf(stderr, "ap=0x%08x limit=0x%08x\n", 
      (int)pcb->allocation_pointer,
      (int)pcb->heap_base+(int)pcb->heap_size-wordsize);
#endif
  return pcb;
}

#define fixnump(x) ((((int)(x)) & fx_mask) == fx_tag)
#define closurep(x) ((((int)(x)) & closure_mask) == closure_tag)
#define immediatep(x) ((((int)(x)) & 7) == 7)
#define tagof(x) (((int) (x)) & 7)
#define ref(x,t) (*((char**)(((char*)(x))+((int)(t)))))
#define align(x) ((((x)+object_alignment-1)>>align_shift)<<align_shift)

typedef struct page_t{
  char* base;
  char* end;
  struct page_t* next;
} page_t;

static page_t* make_page_t(){
  page_t* p = malloc(sizeof(page_t));
  if(p == NULL){
    fprintf(stderr, "failed to allocate page");
    exit(-1);
  }
  return p;
}
  
static void deallocate_string_pages(char* old_string_pages){
  page_t* p;
  p = (page_t*) old_string_pages;
  while(p){
    deallocate_unprotected_space(p->base, p->end - p->base);
    p=p->next;
  }
  p = (page_t*) old_string_pages;
  while(p){
    page_t* n = p->next;
    free(p);
    p = n;
  }
}

#if 0
static char* extend_pointer_ap(pcb_t* pcb, int size){
  if(pcb->pointer_base){
    page_t* p = make_page_t();
    p->base = pcb->pointer_base;
    p->end = pcb->pointer_ap;
    p->next = (page_t*) pcb->pointer_pages;
    pcb->pointer_pages = (char*) p;
  }
  char* ap = allocate_unprotected_space(size);
  pcb->pointer_base = ap;
  pcb->pointer_ap = ap;
  pcb->pointer_eap = ap + size;
  return ap;
}
#endif

static char* alloc_large_string(pcb_t* pcb, int size){
  char* ap = allocate_unprotected_space(size);
  page_t* p = make_page_t();
  p->base = ap;
  p->end = ap+size;
  p->next = (page_t*) pcb->string_pages;
  pcb->string_pages = (char*) p;
  return ap;
}

static char* extend_string_ap(pcb_t* pcb, int size){
  if(pcb->string_base){
    page_t* p = make_page_t();
    p->base = pcb->string_base;
    p->end = pcb->string_ap;
    p->next = (page_t*) pcb->string_pages;
    pcb->string_pages = (char*) p;
  }
  char* ap = allocate_unprotected_space(size);
  pcb->string_base = ap;
  pcb->string_ap = ap;
  pcb->string_eap = ap + size;
  return ap;
}

static char* move_string(char* s, pcb_t* pcb){
  int len = (int) ref(s, -string_tag);
  int sz = align((len>>fx_shift)+disp_string_data+1);
  if(sz < pagesize){
    char* ap = pcb->string_ap;
    char* nap = ap + sz;
    if(nap > pcb->string_eap){
      ap = extend_string_ap(pcb, pagesize);
      pcb->string_eap = ap + pagesize;
      nap = ap + sz;
    }
    pcb->string_ap = nap;
    memcpy(ap, s-string_tag, sz);
    ref(s,-string_tag) = (char*)-1;
    ref(s,wordsize-string_tag) = ap+string_tag;
    return ap + string_tag;
  } 
  else {
    char* ap = alloc_large_string(pcb, sz);
    memcpy(ap, s-string_tag, sz);
    ref(s,-string_tag) = (char*)-1;
    ref(s,wordsize-string_tag) = ap+string_tag;
    return ap + string_tag;
  }
}

static void munch_stack(char* fp, pcb_t* pcb, char* frame_base){
  while(fp != frame_base){
    assert(fp < frame_base);
#if 0
    fprintf(stderr, "copying frame at 0x%08x of 0x%08x\n", 
        (int)fp, (int)stack_base);
#endif
    char* rp = ref(fp, 0);
#if 0
    fprintf(stderr, "return-point = 0x%08x\n", (int)rp);
#endif
    int framesize = (int) ref(rp, disp_frame_size); /* UGLY */
    assert(fixnump(framesize));
    assert(framesize >= 0);
    if(framesize > 0){
      int bytes_in_mask = ((framesize>>fx_shift)+7)>>3;
      char* mask = rp + disp_frame_size - bytes_in_mask;
      fp = fp + framesize;
      char** fpp = (char**) fp;
      int i;
      for(i=0; i<bytes_in_mask; i++){
        unsigned char m = mask[i];
        if(m){
          if (m & 0x01) {
            fpp[0] = move_object(fpp[0], pcb);
          }
          if (m & 0x02) {
            fpp[-1] = move_object(fpp[-1], pcb);
          }
          if (m & 0x04) {
            fpp[-2] = move_object(fpp[-2], pcb);
          }
          if (m & 0x08) {
            fpp[-3] = move_object(fpp[-3], pcb);
          }
          if (m & 0x10) {
            fpp[-4] = move_object(fpp[-4], pcb);
          }
          if (m & 0x20) {
            fpp[-5] = move_object(fpp[-5], pcb);
          }
          if (m & 0x40) {
            fpp[-6] = move_object(fpp[-6], pcb);
          }
          if (m & 0x80) {
            fpp[-7] = move_object(fpp[-7], pcb);
          }
        }
        fpp -= 8;
      }
    }
    else if(framesize == 0){
      framesize = (int)ref(fp, wordsize);
      assert(fixnump(framesize));
      assert(framesize > 0);
#if 0
      /* move cp */
      { 
        char* cp = ref(fp, 2*wordsize);
        assert(closurep(cp));
        ref(fp, 2*wordsize) = move_object(cp, pcb);
      }
#endif
      fp += framesize;
      int i;
      for(i=wordsize; i<(framesize); i+=wordsize){
        ref(fp, -i) = move_object(ref(fp,-i), pcb);
      }
    }
    else {
      fprintf(stderr, "Error: framesize is %d\n", framesize);
      exit(-10);
    }
  }
}

static char* move_stack(char* s, pcb_t* pcb, int sz){
  char* ns;
  int asz = align(sz);
  if(asz < pagesize){
    char* ap = pcb->string_ap;
    char* nap = ap + asz;
    if(nap > pcb->string_eap){
      ap = extend_string_ap(pcb, pagesize);
      pcb->string_eap = ap + pagesize;
      nap = ap + asz;
    }
    pcb->string_ap = nap;
    ns = ap;
  } 
  else {
    ns = alloc_large_string(pcb, asz);
  }
  memcpy(ns, s, sz);
  munch_stack(ns, pcb, ns+sz);
  return ns;
}


static char* move_pointers(char* x, pcb_t* pcb, int size, int tag){
  int sz = align(size);
  char* ap = pcb->allocation_pointer;
  char* nap = ap + sz;
  pcb->allocation_pointer = nap;
  ref(nap, -wordsize) = 0;
  memcpy(ap, x, size);
  ref(x,0) = (char*)-1;
  ref(x,wordsize) = ap + tag;
  return ap + tag;
}


static char* move_continuation(char* x, pcb_t* pcb){
  int sz = (int) ref(x, disp_continuation_size);
  char* top = ref(x, disp_continuation_top);
  char* r = move_pointers(x, pcb, continuation_size, vector_tag);
  ref(r, disp_continuation_top - vector_tag) = move_stack(top, pcb, sz);
  return r;
}

static char* move_object(char* x, pcb_t* pcb){
  if(fixnump(x)){
    return x;
  } 
  else if(immediatep(x)){
    return x;
  }
  else {
    int tag = tagof(x);
    char* fst = ref(x, -tag);
    if(fst == (char*)-1){
      return ref(x,wordsize-tag);
    }
    else if(tag == pair_tag){
      return(move_pointers(x-tag, pcb, pair_size, tag));
    }
    else if(tag == closure_tag){
      assert(ref(fst, -2*wordsize) == 0);
      int size = (int) ref(fst, -wordsize);
      assert(fixnump(size));
      assert(size > 0);
      return (move_pointers(x-tag, pcb, size, tag));
    }
    else if(tag == symbol_tag){
      return (move_pointers(x-tag, pcb, symbol_size, tag));
    }
    else if(tag == vector_tag){
      if(fixnump(fst)){
        return (move_pointers(x-tag, pcb, disp_vector_data + (int)fst, tag));
      }
      else if(fst == (char*)continuation_tag){
        return (move_continuation(x-tag, pcb));
      }
      else {
        fprintf(stderr, "nonvec 0x%08x 0x%08x\n", (int)x, (int)fst);
        exit(-1);
      }
    }
    else if(tag == string_tag){
      return (move_string(x, pcb)); 
    }
    else {
      fprintf(stderr, "here tag=%d\n", tag);
      exit(-1);
    }
  }
}

static void copy_roots(pcb_t* pcb){
  /* first, the constants */
  root_t* r = (root_t*)pcb->roots;
  while(r){
    int n = r->count;
    char** f = r->start;
    int i;
    for(i=0; i<n; i++){
      f[i] = move_object(f[i], pcb);
    }
    r = r->next;
  }

  /* next, the pcb-primitives */
  char** fst = &pcb->scheme_objects;
  char** end = &pcb->scheme_objects_end;
  fst++; 
  while(fst < end){
    *fst = move_object(*fst, pcb);
    fst++;
  }
  /* next, the stack */
  char* fp = pcb->frame_pointer;
  char* frame_base = pcb->frame_base;
  munch_stack(fp, pcb, frame_base);
}



static char* allocate_unprotected_space(int size){
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  char* p = mmap(0, aligned_size,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if(p == MAP_FAILED){
    perror("allocate_unprotected_space failed to mmap");
    exit(-10);
  }
  return p;
}


static void deallocate_unprotected_space(char* p, int size){
  int status;
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  status = munmap(p, aligned_size);
  if(status != 0){
    perror("deallocate_unprotected_space failed to unmap");
    exit(-10);
  }
}


void S_stack_overflow(pcb_t* pcb){
//  fprintf(stderr, "stack overflow detected\n");
  char* stack_top = pcb->stack_top;
  int stack_size = (int) pcb->stack_size;
  char* fp = pcb->frame_pointer;
  char* frame_base = pcb->frame_base;
  assert(fp != frame_base);
  char* rp = ref(fp, 0);
  int framesize = (int) ref(rp, disp_frame_size); /* UGLY */
  assert(fixnump(framesize));
  assert(framesize >= 0);
  if(framesize == 0){
    framesize = (int)ref(fp, wordsize);
    assert(fixnump(framesize));
  }
  // fprintf(stderr, "framesize = %d bytes\n", framesize);
  { /* capture continuation */
    char* next_frame_top = fp + framesize;
    if(next_frame_top == frame_base){
      fprintf(stderr, "continuation already captured\n");
    } else {
      //fprintf(stderr, "capturing continuation ... ");
      char* cont = pcb->allocation_pointer;
      pcb->allocation_pointer += continuation_size;
      ref(cont, 0) = (char*) continuation_tag;
      ref(cont, disp_continuation_top) = next_frame_top;
      ref(cont, disp_continuation_next) = pcb->next_continuation;
      ref(cont, disp_continuation_size) = 
        frame_base - (int)next_frame_top;
      pcb->next_continuation = cont + vector_tag;
      //fprintf(stderr, "done (sz=0x%08x)\n", 
      //    (int) ref(cont, disp_continuation_size));
    }
  }
  int req_stack_size = align_to_page(framesize * 4 + 2 * pagesize);
  if(req_stack_size < minimum_stack_size){
    req_stack_size = minimum_stack_size;
  }
  char* new_stack = allocate_unprotected_space(req_stack_size);
  char* new_frame_redline = new_stack + 2 * pagesize;
  char* new_frame_base = new_stack + req_stack_size - wordsize;
  ref(new_frame_base, 0) = ref(frame_base, 0); /* underflow handler */
  memcpy(new_frame_base - framesize, fp, framesize);
  
  pcb->stack_top = new_stack;
  pcb->stack_size = (char*)req_stack_size;
  pcb->frame_base = new_frame_base;
  pcb->frame_pointer = new_frame_base - framesize;
  pcb->frame_redline = new_frame_redline;
  /*
  fprintf(stderr, "stack=0x%08x .. 0x%08x (redline=0x%08x) fp=0x%08x\n", 
      (int) pcb->frame_base,
      (int) pcb->stack_top,
      (int) pcb->frame_redline,
      (int) pcb->frame_pointer);
  fprintf(stderr, "returning ... \n");
  */
  page_t* p = malloc(sizeof(page_t));
  if(p == NULL){
    fprintf(stderr, "cannot malloc page_t\n");
    exit(-1);
  }
  p->base = stack_top;
  p->end  = stack_top + stack_size;
  p->next = (page_t*) pcb->string_pages;
  pcb->string_pages = (char*) p;
  return;
}

/* 
 On overflow:
 
 +--------------+
 |    unused    |
 |    area      |
 |              |
 +--------------+
 |     rp       |  <-- frame pointer on overflow
 +--------------+
 |   frame      |
 |   when       |
 |   overflow   |
 |   occured    |
 +--------------+
 |   rp_next    |  <-- capture next conitnuation here
 +--------------+      (unless we're at base already)
 |     ...      |
 |     ...      |
 |     ...      |
 +--------------+
 |  underflow   |
 +--------------+

 New stack:
 
 +--------------+
 |    unused    |
 |    area      |
 |              |
 |              |
 |              |
 |              |
 |              |
 |              |
 |              |
 |              |
 +--------------+
 |     rp       |  <-- frame pointer on return
 +--------------+
 |   frame      |
 |   when       |
 |   overflow   |
 |   occured    |
 +--------------+
 |  underflow   |
 +--------------+

 */

