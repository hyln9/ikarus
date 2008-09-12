
#include "ikarus-data.h"
#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>

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

#if 0
ikptr
ikrt_pointer_null(ikptr x /*, ikpcb* pcb*/) {
  return ref(x, off_pointer_data) ? true_object : false_object;
}
#endif

ikptr 
ikrt_dlerror(ikpcb* pcb) {
  char* str = dlerror();
  if (str == NULL) {
    return false_object;
  }
  int len = strlen(str);
  ikptr bv = ik_safe_alloc(pcb, align(disp_bytevector_data + len + 1));
  ref(bv, 0) = fix(len);
  memcpy((void*)(bv+disp_bytevector_data), str, len+1);
  return bv+bytevector_tag;
}

ikptr
ikrt_dlopen(ikptr x, ikptr load_lazy, ikptr load_global, ikpcb* pcb) {
  int flags = 
    ((load_lazy == false_object) ? RTLD_NOW : RTLD_LAZY) |
    ((load_global == false_object) ? RTLD_LOCAL : RTLD_GLOBAL);
  char* name = 
    (x == false_object) 
      ? NULL 
      : (char*)(x + off_bytevector_data);
  void* p = dlopen(name, flags);
  if (p == NULL) {
    return false_object;
  } else {
    return make_pointer((long int) p, pcb);
  }
}

ikptr
ikrt_dlclose(ikptr x /*, ikpcb* pcb*/) {
  int r = dlclose((void*) ref(x, off_pointer_data));
  return (r == 0) ? true_object : false_object;
}


ikptr
ikrt_dlsym(ikptr handle, ikptr sym, ikpcb* pcb) {
  void* p = dlsym((void*)ref(handle, off_pointer_data), 
                  ((char*)sym) + off_bytevector_data);
  if (p == NULL) {
    return false_object;
  } else {
    return make_pointer((long int) p, pcb);
  }
}


ikptr
ikrt_malloc(ikptr len, ikpcb* pcb) {
  void* p = malloc(unfix(len));
  if (p == NULL) {
    return false_object;
  } else {
    return make_pointer((long int) p, pcb);
  }
}

ikptr
ikrt_free(ikptr x) {
  free((void*) ref(x, off_pointer_data));
  return void_object;
}

