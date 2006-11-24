
#include "ikarus.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>   
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <assert.h>

#define forward_ptr ((ikp)-1)
#define DEBUG_STACK 0

#define minimum_heap_size (pagesize * 1024 * 4)
#define maximum_heap_size (pagesize * 1024 * 8)
#define minimum_stack_size (pagesize * 128)

typedef struct qupages_t{
  ikp p;    /* pointer to the scan start */
  ikp q;    /* pointer to the scan end */
  ikp base; /* base of the segment */
  int size; /* and its size */
  struct qupages_t* next;
} qupages_t;

typedef struct {
  ikp ptr_ap;    /* allocation pointer for pointer segment */
  ikp ptr_aq;    /* scan pointer */
  ikp ptr_ep;    /* allocation end for pointer segment */
  ikp ptr_base;  /* base of pointer segment */
  int ptr_size;  /* and size */
  qupages_t* ptr_queue; /* segments queued but not yet fully scanned */
  ikp data_ap;
  ikp data_ep;
  ikp data_base;
  int data_size;
  ikdl codes_queue;
  ikpages* htables_queue;
  ikdl codes_final;
  ikpages* retained_pages;
} gc_t;

#define MIN_PTR_CHUNK (16 * pagesize)
#define MIN_DATA_CHUNK (16 * pagesize)

static
ikp extend_ptr(gc_t* gc, int size){
  if(gc->ptr_size){
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    p->base = gc->ptr_base;
    p->size = gc->ptr_size;
    p->p = gc->ptr_aq; /* start scan where we stopped */
    p->q = gc->ptr_ap; /* and stop at current ap */
    p->next = gc->ptr_queue;
    gc->ptr_queue = p;
  }
  int alloc_size = align_to_next_page(size);
  if(alloc_size < MIN_PTR_CHUNK){
    alloc_size = MIN_PTR_CHUNK;
  }
  ikp p = ik_mmap(alloc_size);
  gc->ptr_ap = p + size;
  gc->ptr_aq = p;
  gc->ptr_ep = p + alloc_size;
  gc->ptr_base = p;
  gc->ptr_size = alloc_size;
  return p;
}

static
ikp extend_data(gc_t* gc, int size){
  if(gc->data_size){
    ikpages* p = ik_malloc(sizeof(ikpages));
    p->base = gc->data_base;
    p->size = gc->data_size;
    p->next = gc->retained_pages;
    gc->retained_pages = p;
  }
  int alloc_size = align_to_next_page(size);
  if(alloc_size < MIN_DATA_CHUNK){
    alloc_size = MIN_DATA_CHUNK;
  }
  ikp p = ik_mmap(alloc_size);
  gc->data_ap = p + size;
  gc->data_ep = p + alloc_size;
  gc->data_base = p;
  gc->data_size = alloc_size;
  return p;
}



static int alloc_ptr_count = 0;

static inline ikp 
alloc_ptr(gc_t* gc, int size){
  assert(size == align(size));
  alloc_ptr_count += size;
  ikp ap = gc->ptr_ap;
  ikp nap = ap + size;
  if(nap > gc->ptr_ep){
    ap = extend_ptr(gc, size);
    //fprintf(stderr, "alloc 0x%08x (size=%d)\n", (int)ap, size);
    return ap;
  } else {
    gc->ptr_ap = nap;
    //fprintf(stderr, "alloc 0x%08x (size=%d)\n", (int)ap, size);
    return ap;
  }
}

static inline 
ikp alloc_data(gc_t* gc, int size){
  assert(size == align(size));
  ikp ap = gc->data_ap;
  ikp nap = ap + size;
  if(nap > gc->data_ep){
    return extend_data(gc, size);
  } else {
    gc->data_ap = nap;
    return ap;
  }
}



static ikp add_object(gc_t* gc, ikp x);
static void collect_stack(gc_t*, ikp top, ikp base);
static void collect_oblist(gc_t*, ikoblist*);
static void collect_loop(gc_t*);
static void mark_codes_dead(ikdl*);
static void deallocate_old_heap(ikpages*);
static void deallocate_old_code(ikdl*);
static void deallocate_dead_hash_tables(ikpcb*);

/* ik_collect is called from scheme under the following conditions:
 * 1. An attempt is made to allocate a small object and the ap is above
 *    the red line.
 * 2. The current frame of the call is dead, so, upon return from ik_collect,
 *    the caller returns to its caller.
 * 3. The frame-pointer of the caller to S_collect is saved at
 *    pcb->frame_pointer.  No variables are live at that frame except for
 *    the return point (at *(pcb->frame_pointer)).
 * 4. S_collect must return a new ap (in pcb->allocation_pointer) that has
 *    at least 2 pages of memory free.
 * 5. S_collect must also update pcb->allocaton_redline to be 2 pages below
 *    the real end of heap.
 * 6. ik_collect should not move the stack.
 */

static void
init_gc(gc_t* gc){
  bzero(gc, sizeof(gc_t));
  ikdl* cq = &gc->codes_queue;
  cq->next = cq; cq->prev = cq;
  ikdl* cf = &gc->codes_final;
  cf->next = cf; cf->prev = cf;
}



ikpcb* ik_collect(int req, ikpcb* pcb);
ikpcb* ik_collect_vararg(int req, ikpcb* pcb){
  fprintf(stderr, "VARARG COLLECT req=%d\n", req);
  return ik_collect(req, pcb);
}

ikpcb* 
ik_collect(int req, ikpcb* pcb){
//  fprintf(stderr, "ik_collect entry %d (pcb=0x%08x)\n", req, (int)pcb);
  gc_t gc;
  init_gc(&gc);

  if(DEBUG_STACK){
    ikp frame_pointer = pcb->frame_pointer;
    ikp frame_base = pcb->frame_base;
    ikp p = frame_pointer;
    while(p != frame_base){
      fprintf(stderr, "stack *0x%08x = 0x%08x\n", (int)p, (int)ref(p,0));
      p+=wordsize;
    }
  }

  mark_codes_dead(&pcb->codes);

  /* the roots are:
   *  1. the stack
   *  2. the next continuation
   *  3. the oblist
   */
  collect_stack(&gc, pcb->frame_pointer, pcb->frame_base - wordsize);
  pcb->next_k = add_object(&gc, pcb->next_k); 
  collect_oblist(&gc, pcb->oblist);

  /* now we trace all live objects */
  collect_loop(&gc);

  /* before deallocating any of the old heap pages, first 
   * deallocate the dead hash_tables */
  deallocate_dead_hash_tables(pcb);

  /* almost there */
  /* the old heap pages in the pcb must bite the dust */
  deallocate_old_heap(pcb->heap_pages);
  pcb->heap_pages = gc.retained_pages;

  deallocate_old_code(&pcb->codes);

  { /* relink code pages to pcb */
    ikdl* x = &gc.codes_final;
    ikdl* y = &pcb->codes;
    ikdl* xn = x->next;
    ikdl* xp = x->prev;
    y->next = xn;
    y->prev = xp;
    xn->prev = y;
    xp->next = y;
  }

  pcb->allocation_pointer = pcb->heap_base;
//  fprintf(stderr, "exiting\n");
//  fprintf(stderr, "heap  base=0x%08x end=0x%08x\n", 
//      (int)pcb->heap_base,
//      (int)pcb->heap_base + pcb->heap_size);
  bzero(pcb->heap_base, pcb->heap_size);
//  memset(pcb->heap_base, -1, pcb->heap_size);
  fprintf(stderr, "allocated %d pages and %d bytes (heap=0x%08x .. 0x%08x) (ht=%d)\n", 
      total_allocated_pages, total_malloced,
      (int)pcb->heap_base, (int)pcb->heap_base+pcb->heap_size,
      hash_table_count);
  return pcb;
}


  
static void
deallocate_dead_hash_tables(ikpcb* pcb){
  ikhashtables* p = pcb->hash_tables;
  ikhashtables* r = 0;
  while(p){
    ikp ht = p->ht;
    ikhashtables* next = p->next;
    if(ref(ht, -vector_tag) == forward_ptr){
      p->ht = ref(ht, wordsize-vector_tag);
      p->next = r;
      r = p;
      p = next;
    } else {
      /* not forwarded, thus dead */
      ikp mem = ref(ht, off_htable_mem);
      int size = (int) ref(ht, off_htable_size);

      ikbucket** table = (ikbucket**) mem;
      int j = size / sizeof(ikbucket*);
      int i;
      for(i=0; i<j; i++){
        ikbucket* b = table[i];
        while(b){
          ikbucket* next = b->next;
          ik_free(b,sizeof(ikbucket));
          b = next;
        }
      }
      ik_munmap(mem, size);
      ik_free(p, sizeof(ikhashtables));
      hash_table_count--;
      p = next;
    }
  }
  pcb->hash_tables = r;
}



static void deallocate_old_heap(ikpages* p){
  while(p){
    ikpages* next = p->next;
    ik_munmap(p->base, p->size);
    ik_free(p, sizeof(ikpages));
    p = next;
  }
}


static void
dl_link(ikdl* x, ikdl* target){
  ikdl* next = target->next;
  x->next = next;
  x->prev = target;
  target->next = x;
  next->prev = x;
}

static void
dl_unlink(ikdl* x){
  ikdl* next = x->next;
  ikdl* prev = x->prev;
  next->prev = prev;
  prev->next = next;
}



static void deallocate_old_code(ikdl* dl){
  ikdl* next = dl->next;
  while(next != dl){
    ikcodes* x = (ikcodes*) next;
    if(x->attr != ikcode_dead){
      fprintf(stderr, "attempt to deallocate live code!\n");
      exit(-1);
    }
    ik_munmap(x->base, x->size);
    dl_unlink(next);
    ik_free(next, sizeof(ikcodes));
    next = dl->next;
  }
}

/* walk the codes in the codes dlink list and set the attr
 * of each one of them to be ikcode_dead 
 */
static void mark_codes_dead(ikdl* dl){
  ikdl* next = dl->next;
  while(next != dl){
    ((ikcodes*)next)->attr = ikcode_dead;
    next = next->next;
  }
}

#define disp_frame_offset -13
#define disp_multivalue_rp -9

static int code_queue_size = 0;

static ikp 
add_code_entry(gc_t* gc, ikp entry){
  ikcode_preheader* ph = 
    (ikcode_preheader*)(entry - disp_code_data - sizeof(ikcode_preheader));
  ikcodes* link = ph->link;
  if(link->attr & ikcode_live){
    return entry;
  } else {
    dl_unlink(&link->dl);
    dl_link(&link->dl, &gc->codes_queue);
    link->attr = ikcode_queued;
    code_queue_size++;
//    fprintf(stderr, "queued codes = %d\n", ++queued_code_count);
    return entry;
  }
}

static void collect_oblist(gc_t* gc, ikoblist* st){
  ikbucket** p = st->buckets;
  ikbucket** q = p + st->number_of_buckets;
  while(p < q){
    ikbucket* b = *p;
    while(b){
      b->val = add_object(gc, b->val);
      b = b->next;
    }
    p++;
  }
}

#define DEBUG_STACK 0

static void collect_stack(gc_t* gc, ikp top, ikp end){
  if(DEBUG_STACK){
    fprintf(stderr, "collecting stack from 0x%08x .. 0x%08x\n", 
        (int) top, (int) end);
  }
  while(top < end){
    if(DEBUG_STACK){
      fprintf(stderr, "collecting frame at 0x%08x: ", (int) top);
    }
    ikp rp = ref(top, 0);
    int rp_offset = unfix(ref(rp, disp_frame_offset));
    if(DEBUG_STACK){
      fprintf(stderr, "rp_offset=%d\n", rp_offset);
    }
    if(rp_offset <= 0){
      fprintf(stderr, "invalid rp_offset %d\n", rp_offset);
      exit(-1);
    }

    /* since the return point is alive, we need to find the code
     * object containing it and mark it live as well.  the rp is
     * updated to reflect the new code object. */

    int code_offset = rp_offset - disp_frame_offset;
    ikp code_entry = rp - code_offset;
    ikp new_code_entry = add_code_entry(gc, code_entry);
    ikp new_rp = new_code_entry + code_offset;
    ref(top, 0) = new_rp;

    /* now for some livemask action.
     * every return point has a live mark above it.  the live mask
     * is a sequence of bytes (every byte for 8 frame cells).  the 
     * size of the live mask is determined by the size of the frame.
     * this is how the call frame instruction sequence looks like:
     *
     *   |    ...     |
     *   | code  junk |
     *   +------------+
     *   |   byte 0   |   for fv0 .. fv7
     *   |   byte 1   |   for fv8 .. fv15
     *   |    ...     |   ...
     *   +------------+
     *   |  framesize |
     *   |    word    |
     *   +------------+
     *   | multivalue |
     *   |    word    |
     *   +------------+
     *   | frameoffst |  the frame offset determined how far its
     *   |    word    |  address is off from the start of the code
     *   +------------+
     *   |  padding   |  the size of this part is fixed so that we
     *   |  and call  |  can correlate the frame info (above) with rp
     *   +------------+
     *   | code  junk | <---- rp
     *   |    ...     | 
     *
     *   WITH ONE EXCEPTION:
     *   if the framesize is 0, then the actual frame size is stored
     *   on the stack immediately below the return point.
     *   there is no live mask in this case, instead all values in the
     *   frame are live.
     */
    int framesize = (int) ref(rp, disp_frame_size);
    if(DEBUG_STACK){
      fprintf(stderr, "fs=%d\n", framesize);
    }
    if(framesize < 0){
      fprintf(stderr, "invalid frame size %d\n", framesize);
      exit(-1);
    } 
    else if(framesize == 0){
      framesize = (int)ref(top, wordsize);
      fprintf(stderr, "special frame of size %d\n", framesize);
      if(framesize <= 0){
        fprintf(stderr, "invalid redirected framesize=%d\n", framesize);
        exit(-1);
      }
      ikp base = top + framesize - wordsize;
      while(base > top){
        fprintf(stderr, "obj at 0x%08x = 0x%08x\n",
            (int)base, (int)ref(base,0));
        ikp new_obj = add_object(gc,ref(base,0));
        ref(base,0) = new_obj;
        if(tagof(new_obj) == string_tag){
          fprintf(stderr, "STRING %s\n", string_data(new_obj));
        }
        base -= wordsize;
      }
    } else {
      int frame_cells = framesize >> fx_shift;
      int bytes_in_mask = (frame_cells+7) >> 3;
      unsigned char* mask = rp + disp_frame_size  - bytes_in_mask;
  
      ikp* fp = (ikp*)(top + framesize);
      int i;
      for(i=0; i<bytes_in_mask; i++, fp-=8){
        unsigned char m = mask[i];
        if(m & 0x01) { fp[-0] = add_object(gc, fp[-0]); }
        if(m & 0x02) { fp[-1] = add_object(gc, fp[-1]); }
        if(m & 0x04) { fp[-2] = add_object(gc, fp[-2]); }
        if(m & 0x08) { fp[-3] = add_object(gc, fp[-3]); }
        if(m & 0x10) { fp[-4] = add_object(gc, fp[-4]); }
        if(m & 0x20) { fp[-5] = add_object(gc, fp[-5]); }
        if(m & 0x40) { fp[-6] = add_object(gc, fp[-6]); }
        if(m & 0x80) { fp[-7] = add_object(gc, fp[-7]); }
      }
    }
    top += framesize;
  }
  if(top != end){
    fprintf(stderr, "frames did not match up 0x%08x .. 0x%08x\n",
        (int) top, (int) end);
    exit(-1);
  }
  if(DEBUG_STACK){
    fprintf(stderr, "done with stack!\n");
  }
}

static ikp 
add_object(gc_t* gc, ikp x){
  if(is_fixnum(x)){ 
    return x;
  } 
  if(x == forward_ptr){
    fprintf(stderr, "GOTCHA\n");
    exit(-1);
  }
  int tag = tagof(x);
  if(tag == immediate_tag){
    return x;
  }
  ikp fst = ref(x, -tag);
  if(fst == forward_ptr){
    /* already moved */
    return ref(x, wordsize-tag);
  }
  if(tag == pair_tag){
    ikp snd = ref(x, off_cdr);
    ikp y = alloc_ptr(gc,pair_size) + pair_tag;
    ref(y,off_car) = fst;
    ref(y,off_cdr) = snd;
    ref(x,off_car) = forward_ptr;
    ref(x,off_cdr) = y;
    return y;
  }
  else if(tag == symbol_tag){
    ikp y = alloc_ptr(gc, symbol_size) + symbol_tag;
    ref(y, off_symbol_string) = ref(x, off_symbol_string);
    ref(y, off_symbol_ustring) = ref(x, off_symbol_ustring);
    ref(y, off_symbol_value) = ref(x, off_symbol_value);
    ref(y, off_symbol_plist) = ref(x, off_symbol_plist);
    ref(y, off_symbol_system_value) = ref(x, off_symbol_system_value);
    ref(y, off_symbol_system_plist) = ref(x, off_symbol_system_plist);
    ref(x, -symbol_tag) = forward_ptr;
    ref(x, wordsize-symbol_tag) = y;
    return y;
  }
  else if(tag == closure_tag){
    int size = (int) ref(fst, disp_code_closure_size - disp_code_data);
    if(size <= 0){
      fprintf(stderr, "invalid closure size=%d\n", size);
      exit(-1);
    }
    if(size > 1024){
      fprintf(stderr, "large closure size=0x%08x\n", size);
    }
    int asize = align(size);
    ikp y = alloc_ptr(gc, asize) + closure_tag;
    bzero(y-closure_tag, asize);
    memcpy(y-closure_tag, x-closure_tag, size);
    ref(y,-closure_tag) = add_code_entry(gc, ref(y,-closure_tag));
    ref(x,-closure_tag) = forward_ptr;
    ref(x,wordsize-closure_tag) = y;
    return y;
  }
  else if(tag == vector_tag){
    if(is_fixnum(fst)){
      /* real vector */
      int size = (int)fst;
      if(size > 4096){
        fprintf(stderr, "large vec size=0x%08x\n", size);
      }
      int memreq = align(size + disp_vector_data);
      ikp y = alloc_ptr(gc, memreq) + vector_tag;
      bzero(y-vector_tag, memreq);
      memcpy(y-vector_tag, x-vector_tag, size + disp_vector_data);
      ref(x,-vector_tag) = forward_ptr;
      ref(x,wordsize-vector_tag) = y;
      return y;
    } 
    else if(tagof(fst) == rtd_tag){
      /* record */
      int size = (int) ref(fst, off_rtd_length);
      if(size > 4096){
        fprintf(stderr, "large rec size=0x%08x\n", size);
      }
      int memreq = align(size + disp_record_data);
      ikp y = alloc_ptr(gc, memreq) + vector_tag;
      bzero(y-vector_tag, memreq);
      memcpy(y-vector_tag, x-vector_tag, size+wordsize);
      ref(x,-vector_tag) = forward_ptr;
      ref(x,wordsize-vector_tag) = y;
      return y;
    }
    else if(fst == code_tag){
      ikp entry = x + off_code_data;
      ikp new_entry = add_code_entry(gc, entry);
      return new_entry - off_code_data;
    } 
    else if(fst == continuation_tag){
//      fprintf(stderr, "conitnuation!\n");
      ikp top = ref(x, off_continuation_top);
      int size = (int) ref(x, off_continuation_size);
      if(size > 4096){
        fprintf(stderr, "large cont size=0x%08x\n", size);
      }
      ikp next = ref(x, off_continuation_next);
      ikp y = alloc_ptr(gc, continuation_size) + vector_tag;
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ikp new_top = alloc_data(gc, align(size));
      memcpy(new_top, top, size);
      collect_stack(gc, new_top, new_top + size);
      ref(y, -vector_tag) = continuation_tag;
      ref(y, off_continuation_top) = new_top;
      ref(y, off_continuation_size) = (ikp) size;
      ref(y, off_continuation_next) = next;
      return y;
    }
    else if(fst == htable_tag){
      ikp count = ref(x, off_htable_count);
      ikp size = ref(x, off_htable_size);
      ikp mem = ref(x, off_htable_mem);
      ikp y = alloc_ptr(gc, htable_size) + vector_tag;
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y,-vector_tag) = htable_tag;
      ref(y, off_htable_count) = count;
      ref(y, off_htable_size) = size;
      ref(y, off_htable_mem) = mem;
      if(size){
        ikpages* p = ik_malloc(sizeof(ikpages));
        p->base = mem;
        p->size = (int)size;
        p->next = gc->htables_queue;
        gc->htables_queue = p;
      }
      return y;
    }
    else {
      fprintf(stderr, "unhandled vector with fst=0x%08x\n", (int)fst);
      exit(-1);
    }
  }
  else if(tag == string_tag){
    if(is_fixnum(fst)){
      int strlen = unfix(fst);
      if(strlen > 4096){
        fprintf(stderr, "large string size=0x%08x\n", strlen);
      }
      int memreq = align(strlen + disp_string_data + 1);
      ikp new_str = alloc_data(gc, memreq) + string_tag;
      ref(new_str, off_string_length) = fst;
      memcpy(new_str+off_string_data,
             x + off_string_data,
             strlen + 1);
      ref(x, -string_tag) = forward_ptr;
      ref(x, wordsize-string_tag) = new_str;
      return new_str;
    }
    else {
      fprintf(stderr, "unhandled string with fst=0x%08x\n", (int)fst);
      exit(-1);
    }
  }
  fprintf(stderr, "unhandled tag: %d\n", tag);
  exit(-1);
}

static void 
scan_code(gc_t* gc, ikp x){
  code_queue_size--;
//  fprintf(stderr, "scan_code %d\n", ++scan_code_count);
  int instrsize = (int)ref(x, disp_code_code_size);
  int relocsize = (int)ref(x, disp_code_reloc_size);
  ikp p = x + disp_code_data + instrsize;
  ikp pe = p + relocsize;
  while(p < pe){
    int r = (int) ref(p,0);
    if(r == 0){
      p = pe;
    }
    else {
      int rtag = r & 3;
      if(rtag == 0){
        /* undisplaced pointer */
        int code_offset = r >> 2;
        ikp old_object = ref(x, disp_code_data + code_offset);
        ikp new_object = add_object(gc, old_object);
        ref(x, disp_code_data + code_offset) = new_object;
        p += wordsize;
      }
      else if(rtag == 1){
        /* displaced pointer */
        int code_offset = r >> 2;
        int object_offset = (int) ref(p, wordsize);
        ikp old_displaced_object = ref(x, disp_code_data + code_offset);
        ikp old_object = old_displaced_object - object_offset;
        ikp new_object = add_object(gc, old_object);
        ikp new_displaced_object = new_object + object_offset;
        ref(x, disp_code_data + code_offset) = new_displaced_object;
        p += (2 * wordsize);
      } 
      else if(rtag == 2){
        /* displaced relative pointer */
        int code_offset = r >> 2;
        int relative_offset = (int) ref(p, wordsize);
        ikp old_relative_pointer = ref(x, disp_code_data + code_offset);
        ikp old_relative_object = old_relative_pointer - relative_offset;
        ikp old_addr = x + disp_code_data + code_offset + wordsize;
        ikp old_object = old_relative_object + (unsigned int) old_addr;
        ikp new_object = add_object(gc, old_object);
        ikp new_disp_object = new_object + relative_offset;
        ikp next_word = x + disp_code_data + code_offset + wordsize;
        ikp new_relative_pointer = 
          new_disp_object - (unsigned int) next_word;
        ref(next_word, -wordsize) = new_relative_pointer;
        p += (2 * wordsize);
      }
      else if(rtag == 3){
        /* foreign object */
        /* just add the name */
        ref(p, wordsize) = add_object(gc, ref(p, wordsize));
        p += (2 * wordsize);
      }
      else {
        fprintf(stderr, "invalid rtag %d in 0x%08x\n", rtag, r);
        exit(-1);
      }
    } 
  }
}


static void
rehash_hash_table(gc_t* gc, ikbucket** table, int size){
  /* first unlink all the buckets from the table and place
   * them in one linked list, then relink all of them again 
   * in their new positions */
  ikbucket* q = 0;
  ikbucket** last_bucket = &q;
  int i;
  int count = 0;
  for(i=0; i<size; i++){
    ikbucket* b = table[i];
    if(b){
      *last_bucket = b;
      table[i] = 0;
      count++;
      ikbucket* next = b->next;
      while(next){
        count++;
        b = next;
        next = next->next;
      }
      last_bucket = &b->next;
    }
  }
  while(q){
    count--;
    ikbucket* next = q->next;
    ikp new_key = add_object(gc, q->key);
    ikp new_val = add_object(gc, q->val);
    q->key = new_key;
    q->val = new_val;
    int idx = inthash(new_key) & (size-1);
    q->next = table[idx];
    table[idx] = q;
    q = next;
  }
  assert(count == 0);
}


static void 
collect_loop(gc_t* gc){
  int done;
  do{
    done = 1;
    { /* scan the pending pointer pages */
      qupages_t* qu = gc->ptr_queue;
      if(qu){
        fprintf(stderr, "PTRQUEUE\n");
        done = 0;
        gc->ptr_queue = 0;
        do{
          ikp p = qu->p;
          ikp q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0));
            alloc_ptr_count -= wordsize;
            p += wordsize;
          }
          qupages_t* next = qu->next;
          ikpages* f = ik_malloc(sizeof(ikpages));
          f->base = qu->base;
          f->size = qu->size;
          f->next = gc->retained_pages;
          gc->retained_pages = f;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending code objects */
      ikdl* codeq = &gc->codes_queue;
      ikdl* next = codeq->next;
      if(next != codeq){
        done = 0;
        do{
          dl_unlink(next);
          dl_link(next, &gc->codes_final);
          ikcodes* s = (ikcodes*)next;
          if(s->attr != ikcode_queued){
            fprintf(stderr, "code is not queued!\n");
            exit(-1);
          }
          s->attr = ikcode_scanned;
          if(mprotect(s->base, s->size, PROT_READ | PROT_WRITE) != 0){
            fprintf(stderr, "Error unprotecting code page!");
            exit(-1);
          }
          scan_code(gc, s->code_object);
          if(mprotect(s->base, s->size, PROT_READ | PROT_EXEC) != 0){
            fprintf(stderr, "Error protecting code page!");
            exit(-1);
          }
          next = codeq->next;
        } while(next != codeq);
      }
    }

    {
      ikpages* q = gc->htables_queue;
      if(q){
        done = 0;
        gc->htables_queue = 0;
        do{
          ikp base = q->base;
          int size = q->size;
          ikpages* next = q->next;
          ik_free(q, sizeof(ikpages));
          rehash_hash_table(gc, (ikbucket**)base, size/sizeof(ikbucket*));
          q = next;
        } while (q);
      }
    }

    {/* see if there are any remaining in the main ptr segment */
      ikp p = gc->ptr_aq;
      ikp q = gc->ptr_ap;
      if(p < q){
        done = 0;
//        do{
          gc->ptr_aq = q;
          while(p < q){
            //fprintf(stderr, "ADD 0x%08x (0x%08x)\n", (int)p, (int)ref(p,0));
            ref(p,0) = add_object(gc, ref(p,0));
            alloc_ptr_count -= wordsize;
            p += wordsize;
          }
//          p = gc->ptr_aq;
//          q = gc->ptr_ap;
//        } while (p < q);
      }
    }
    /* phew */
  } while (! done);
  /* now that we're done, any remaining data in gc (ptr, data) should
   * be moved to the retained_pages.
   */
  if(gc->ptr_ap){
    ikpages* p = ik_malloc(sizeof(ikpages));
    p->base = gc->ptr_base;
    p->size = gc->ptr_size;
    p->next = gc->retained_pages;
    gc->retained_pages = p;
  }
  if(gc->data_ap){
    ikpages* p = ik_malloc(sizeof(ikpages));
    p->base = gc->data_base;
    p->size = gc->data_size;
    p->next = gc->retained_pages;
    gc->retained_pages = p;
  }
  if(alloc_ptr_count != 0){
    fprintf(stderr, "ERROR: %d not scanned\n", alloc_ptr_count);
    exit(-1);
  }
  if(code_queue_size != 0){
    fprintf(stderr, "ERROR: %d codes not scanned\n", code_queue_size);
    exit(-1);
  }

}

