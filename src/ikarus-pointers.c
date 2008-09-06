
#include "ikarus-data.h"

ikptr
ikrt_isapointer(ikptr x, ikpcb* pcb){
  if ((tagof(x) == vector_tag) && (ref(x, -vector_tag) == pointer_tag)) {
    return true_object;
  } else {
    return false_object;
  }
}

ikptr
ikrt_pointer_to_int(ikptr x, ikpcb* pcb) {
  long int p = (long int) ref(x, wordsize-vector_tag);
  ikptr pfx = fix(p);
  if (unfix(pfx) == p) {
    return pfx;
  } else {
    ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
    if (p > 0){
      ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift)); 
      ref(bn, disp_bignum_data) = (ikptr)p;
    } else {
      ref(bn, 0) = 
        (ikptr)(bignum_tag | 
              (1 << bignum_length_shift) | 
              (1 << bignum_sign_shift));
      ref(bn, disp_bignum_data) = (ikptr)-p;
    } 
    return bn+vector_tag;
  }
}

static ikptr
make_pointer(long int x, ikpcb* pcb) {
  ikptr r = ik_safe_alloc(pcb, pointer_size);
  ref(r, 0) = pointer_tag;
  ref(r, wordsize) = (ikptr)x;
  return r+vector_tag;
}

ikptr
ikrt_fx_to_pointer(ikptr x, ikpcb* pcb) {
  return make_pointer(unfix(x), pcb);
}

#define bnfst_negative(x) \
  (((unsigned long int)(x)) & bignum_sign_mask)
ikptr
ikrt_bn_to_pointer(ikptr x, ikpcb* pcb) {
  if(bnfst_negative(ref(x, -vector_tag))){
    return make_pointer(-ref(x, wordsize-vector_tag), pcb);
  } else {
    return make_pointer(ref(x, wordsize-vector_tag), pcb);
  }
}

