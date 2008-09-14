
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

ikptr
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


ikptr
ikrt_ref_char(ikptr p, ikptr off /*, ikpcb* pcb*/) {
  return fix(*((signed char*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}

ikptr
ikrt_ref_uchar(ikptr p, ikptr off /*, ikpcb* pcb*/) {
  return fix(*((unsigned char*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}

ikptr
ikrt_ref_short(ikptr p, ikptr off /*, ikpcb* pcb*/) {
  return fix(*((signed short*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}

ikptr
ikrt_ref_ushort(ikptr p, ikptr off /*, ikpcb* pcb*/) {
  return fix(*((unsigned short*)(((long)ref(p, off_pointer_data)) + unfix(off))));
}

ikptr
s_to_number(signed long n, ikpcb* pcb) {
  ikptr fx = fix(n);
  if (unfix(fx) == n) {
    return fx;
  }
  ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
  if (n > 0){
    ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift)); 
    ref(bn, disp_bignum_data) = (ikptr)n;
  } else {
    ref(bn, 0) = 
      (ikptr)(bignum_tag | 
            (1 << bignum_length_shift) | 
            (1 << bignum_sign_shift));
    ref(bn, disp_bignum_data) = (ikptr)-n;
  }
  return bn+vector_tag;
}
  
ikptr
u_to_number(unsigned long n, ikpcb* pcb) {
  unsigned long mxn = ((unsigned long)-1)>>(fx_shift+1);
  if (n <= mxn) {
    return fix(n);
  }
  ikptr bn = ik_safe_alloc(pcb, align(wordsize+disp_bignum_data));
  ref(bn, 0) = (ikptr)(bignum_tag | (1 << bignum_length_shift)); 
  ref(bn, disp_bignum_data) = (ikptr)n;
  return bn+vector_tag;
}

ikptr
d_to_number(double n, ikpcb* pcb) {
  ikptr x = ik_safe_alloc(pcb, flonum_size) + vector_tag;
  ref(x, -vector_tag) = flonum_tag;
  flonum_data(x) = n;
  return x;
}



ikptr
ikrt_ref_int(ikptr p, ikptr off , ikpcb* pcb) {
  signed int r = 
    *((signed int*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  if (wordsize == 8) {
    return fix(r);
  } else {
    return s_to_number(r, pcb);
  }
}

ikptr
ikrt_ref_uint(ikptr p, ikptr off , ikpcb* pcb) {
  unsigned int r = 
    *((unsigned int*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  if (wordsize == 8) {
    return fix(r);
  } else {
    return u_to_number(r, pcb);
  }
}

ikptr
ikrt_ref_long(ikptr p, ikptr off , ikpcb* pcb) {
  signed long r = 
    *((signed long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return s_to_number(r, pcb);
}

ikptr
ikrt_ref_ulong(ikptr p, ikptr off , ikpcb* pcb) {
  unsigned long r = 
    *((unsigned long*)(((long)ref(p, off_pointer_data)) + unfix(off)));
  return u_to_number(r, pcb);
}

long
extract_num(ikptr x) {
  if (is_fixnum(x)) {
    return unfix(x);
  } else {
    if(bnfst_negative(ref(x, -vector_tag))){
      return (long)(-ref(x, wordsize-vector_tag));
    } else {
      return (long)(ref(x, wordsize-vector_tag));
    }
  }
}

ikptr
ikrt_set_char(ikptr p, ikptr off, ikptr v/*, ikpcb* pcb*/) {
  *((signed char*)(((long)ref(p, off_pointer_data)) + unfix(off))) = 
    extract_num(v);
  return void_object;
}

ikptr
ikrt_set_short(ikptr p, ikptr off, ikptr v/*, ikpcb* pcb*/) {
  *((signed short*)(((long)ref(p, off_pointer_data)) + unfix(off))) = 
    extract_num(v);
  return void_object;
}

ikptr
ikrt_set_int(ikptr p, ikptr off, ikptr v/*, ikpcb* pcb*/) {
  *((signed int*)(((long)ref(p, off_pointer_data)) + unfix(off))) = 
    extract_num(v);
  return void_object;
}

ikptr
ikrt_set_long(ikptr p, ikptr off, ikptr v/*, ikpcb* pcb*/) {
  *((signed long*)(((long)ref(p, off_pointer_data)) + unfix(off))) = 
    extract_num(v);
  return void_object;
}


