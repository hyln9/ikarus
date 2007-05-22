
#include "ikarus.h"
#include <stdlib.h>
#include <assert.h>

static int
page_idx(unsigned char* x){
  unsigned int xi = (unsigned int) x;
  return xi >> pageshift;
}

#define fixnum_mask 3
#define pmask 7


#ifndef NDEBUG
static int isa_fixnum(ikp x){
  return ((fixnum_mask & (int)x) == 0);
}

static int isa_vector(ikp x){
  return ( (tagof(x) == vector_tag) &&
           isa_fixnum(ref(x, -vector_tag)));
}
#endif




static void 
verify_code(unsigned char* x, unsigned char* base, unsigned int* svec, unsigned int* dvec){
  assert(ref(x, 0) == code_tag);
  ikp rvec = ref(x, disp_code_reloc_vector);
  assert(isa_vector(rvec));
  ikp codesize = ref(x, disp_code_code_size);
  codesize += 0;
  assert(unfix(codesize) >= 0);
  assert(isa_fixnum(codesize));
  ikp freevars = ref(x, disp_code_freevars);
  freevars += 0;
  assert(isa_fixnum(freevars));
  assert(unfix(freevars) >= 0);

  unsigned int rs = svec[page_idx(rvec) - page_idx(base)];
  unsigned int cs = svec[page_idx(x) - page_idx(base)];
  int cgen = cs&gen_mask;
  int rgen = rs&gen_mask;
  if(rgen < cgen){
    unsigned int d = dvec[page_idx(x) - page_idx(base)];
    d = d & d;
    //int off = (((int)x) - align_to_prev_page(x)) / card_size;
    //int card_mark = (d >> off) & 0xF;
    assert(d != 0);
  }
}

static void 
verify_object(ikp x, unsigned char* base, unsigned int* svec, unsigned int* dvec){
  
}


static unsigned char*
verify_code_small(unsigned char* p, unsigned int s, unsigned int d, 
    unsigned char* base, unsigned int* svec, unsigned int* dvec){
  ikp q = p + pagesize;
  while(p < q){
    ikp fst = ref(p, 0);
    if(fst == code_tag){
      assert(is_fixnum(ref(p, disp_code_code_size)));
      int code_size = unfix(ref(p, disp_code_code_size));
      assert(code_size >= 0);
      verify_code(p, base, svec, dvec);
      p+=align(code_size + disp_code_data);
    } else {
      p = q;
    }
  }
  if(p != q){
    fprintf(stderr, "code extended beyond a page in %p, %p\n", p, q);
    assert(0);
  }
  return q;
}

static unsigned char*
verify_code_large(unsigned char* p, unsigned int s, unsigned int d, 
    unsigned char* base, unsigned int* svec, unsigned int* dvec){
  ikp fst = ref(p, 0);
  fst += 0;
  assert(fst == code_tag);
  int code_size = unfix(ref(p, disp_code_code_size));
  assert(code_size >= 0);
  verify_code(p, base, svec, dvec);
  assert(align(code_size+disp_code_data) >= pagesize);
  ikp end = p + code_size + disp_code_data;
  return((unsigned char*)align_to_next_page(end));
}

static unsigned char*
verify_code_page(unsigned char* p, unsigned int s, unsigned int d, 
    unsigned char* base, unsigned int* svec, unsigned int* dvec){
  ikp fst = ref(p, 0);
  fst += 0;
  assert (fst == code_tag);
  int code_size = unfix(ref(p, disp_code_code_size));
  assert(code_size >= 0);
  int obj_size = align(code_size + disp_code_data);
  unsigned char* result;
  if(obj_size <= pagesize){
    result = verify_code_small(p,s,d,base,svec,dvec);
  } else {
    result = verify_code_large(p,s,d,base,svec,dvec);
  }
 // fprintf(stderr, "code verify incomplete\n");
  return result;
}


      

static unsigned char*
verify_pointers_page(unsigned char* p, unsigned int s, unsigned int d, 
    unsigned char* base, unsigned int* svec, unsigned int* dvec){
  {
    int i = 0;
    while(i < pagesize){
      verify_object(ref(p, i), base, svec, dvec);
      i += wordsize;
    }
  }
  //fprintf(stderr, "pointers verif incomplete\n");
  return p+pagesize; 
}

static unsigned char*
verify_page(unsigned char* p, unsigned char* base, unsigned int* svec, unsigned int* dvec){
  int idx = page_idx(p) - page_idx(base);
  unsigned int s = svec[idx];
  unsigned int d = dvec[idx];
//  if(s & dealloc_mask){
//    return p+pagesize;
//  }
  int type = s & type_mask;
  if(type == hole_type){
    return p+pagesize;
  }
  assert((s & new_gen_mask) == 0);
  if(type == code_type){
    return verify_code_page(p,s,d,base,svec,dvec);
  }
  else if(type == pointers_type){
    return verify_pointers_page(p,s,d,base,svec,dvec);
  }
  else if(type == symbols_type){
    return verify_pointers_page(p,s,d,base,svec,dvec);
  }
  else if(type == dat_type){
    /* nothing to do for data */
    return p+pagesize;
  }
  else if(type == mainheap_type){
    /* nothing to do for main heap */
    return p+pagesize;
  }
  else if(type == mainstack_type){
    /* nothing to do for main stack */
    return p+pagesize;
  }
  fprintf(stderr, "type=0x%08x\n", type);
  exit(-1);
}

void
verify_integrity(ikpcb* pcb, char* where){
  fprintf(stderr, "verifying in %s...\n", where);
  unsigned char* mem_base = pcb->memory_base;
  unsigned char* mem_end = pcb->memory_end;
  unsigned int* seg_vec = pcb->segment_vector_base;
  unsigned int* dir_vec = pcb->dirty_vector_base;
  unsigned char* mem = mem_base;
  while(mem < mem_end){
    mem = verify_page(mem, mem_base, seg_vec, dir_vec);
  }
  fprintf(stderr, "verify_ok in %s\n", where);
}
