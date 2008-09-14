
#include "ikarus-data.h"
#include "config.h"

#if ENABLE_LIBFFI
#include <ffi.h>
#include <stdlib.h>
#include <strings.h>

static void*
alloc(size_t n, int m) {
  void* x = calloc(n, m);
  if (x == NULL) {
    fprintf(stderr, "ERROR (ikarus): calloc failed!\n");
    exit(-1);
  }
  return x;
}

static ffi_type* 
scheme_to_ffi_type_cast(int n){
  switch (n & 0xF) {
    case  1: return &ffi_type_void;
    case  2: return &ffi_type_uint8;
    case  3: return &ffi_type_sint8;
    case  4: return &ffi_type_uint16;
    case  5: return &ffi_type_sint16;
    case  6: return &ffi_type_uint32;
    case  7: return &ffi_type_sint32;
    case  8: return &ffi_type_uint64;
    case  9: return &ffi_type_sint64;
    case 10: return &ffi_type_float;
    case 11: return &ffi_type_double;
    case 12: return &ffi_type_pointer;
    default: 
      fprintf(stderr, "INVALID ARG %d", n);
      exit(-1);
  }
}

static void* 
alloc_room_for_type(int n){
  ffi_type* t = scheme_to_ffi_type_cast(n);
  return alloc(t->size, 1);
}

extern long extract_num(ikptr x);

static void*
scheme_to_ffi_value_cast(int n, ikptr p) {
  void* r = alloc_room_for_type(n);
  switch (n & 0xF) {
    case  1: { free(r);  return NULL; }
    case  2: // ffi_type_uint8;
    case  3:
     { *((char*)r) = extract_num(p); return r; }
    case  4: // ffi_type_uint16;
    case  5: 
     { *((short*)r) = extract_num(p); return r; }
    case  6: //  ffi_type_uint32;
    case  7: 
     { *((int*)r) = extract_num(p); return r; }
    case  8: // ffi_type_uint64;
    case  9: 
     { *((long*)r) = extract_num(p); return r; }
    case 10: //return &ffi_type_float;
     { *((float*)r) = flonum_data(p); return r; }
    case 11: //return &ffi_type_double;
     { *((double*)r) = flonum_data(p); return r; }
    case 12: //return &ffi_type_pointer;
     { *((void**)r) = (void*)ref(p, off_pointer_data); return r; }
    default: 
      fprintf(stderr, "INVALID ARG %d", n);
      exit(-1);
  }
}


extern ikptr u_to_number(unsigned long x, ikpcb* pcb);
extern ikptr s_to_number(signed long x, ikpcb* pcb);
extern ikptr d_to_number(double x, ikpcb* pcb);
extern ikptr make_pointer(void* x, ikpcb* pcb);
static ikptr
ffi_to_scheme_value_cast(int n, void* p, ikpcb* pcb) {
  switch (n & 0xF) {
    case  1: return void_object; 
    case  2: return u_to_number(*((unsigned char*)p), pcb);
    case  3: return s_to_number(*((signed char*)p), pcb);
    case  4: return u_to_number(*((unsigned short*)p), pcb);
    case  5: return s_to_number(*((signed short*)p), pcb);
    case  6: return u_to_number(*((unsigned int*)p), pcb);
    case  7: return s_to_number(*((signed int*)p), pcb);
    case  8: return u_to_number(*((unsigned long*)p), pcb);
    case  9: return s_to_number(*((signed long*)p), pcb);
    case 10: return d_to_number(*((float*)p), pcb);
    case 11: return d_to_number(*((double*)p), pcb);
    case 12: return make_pointer(*((void**)p), pcb);
    default: 
      fprintf(stderr, "INVALID ARG %d", n);
      exit(-1);
  }
}

ikptr
ikrt_ffi_prep_cif(ikptr rtptr, ikptr argstptr, ikpcb* pcb) {
  ffi_cif* cif = alloc(sizeof(ffi_cif), 1);
  bzero(cif, sizeof(ffi_cif));
  ffi_abi abi = FFI_DEFAULT_ABI;
  unsigned int nargs = unfix(ref(argstptr, off_vector_length));
  ffi_type** argtypes = alloc(sizeof(ffi_type*), nargs);
  int i;
  for(i=0; i<nargs; i++){
    ikptr argt = ref(argstptr, off_vector_data + i*wordsize);
    argtypes[i] = scheme_to_ffi_type_cast(unfix(argt));
  }
  ffi_type* rtype = scheme_to_ffi_type_cast(unfix(rtptr));
  ffi_status s = ffi_prep_cif(cif, abi, nargs, rtype, argtypes);
  if (s == FFI_OK) {
    ikptr r = ik_safe_alloc(pcb, pointer_size);
    ref(r, 0) = pointer_tag;
    ref(r, wordsize) = (ikptr)cif;
    return (r + vector_tag);
  } else {
    return false_object;  
  }
}



ikptr
ikrt_ffi_call(ikptr data, ikptr argsvec, ikpcb* pcb)  {
  ikptr cifptr  = ref(data, off_vector_data + 0 * wordsize);
  ikptr funptr  = ref(data, off_vector_data + 1 * wordsize);
  ikptr typevec = ref(data, off_vector_data + 2 * wordsize);
  ikptr rtype   = ref(data, off_vector_data + 3 * wordsize);
  ffi_cif* cif = (ffi_cif*) ref(cifptr, off_pointer_data);
  void* fn = (void*) ref(funptr, off_pointer_data);
  unsigned int n = unfix(ref(argsvec, off_vector_length));
  void** avalues = alloc(sizeof(void*), n);
  int i;
  for(i=0; i<n; i++){
    ikptr t = ref(typevec, off_vector_data + i * wordsize);
    ikptr v = ref(argsvec, off_vector_data + i * wordsize);
    avalues[i] = scheme_to_ffi_value_cast(unfix(t), v);
  }
  void* rvalue = alloc_room_for_type(unfix(rtype));;
  ffi_call(cif, fn, rvalue, avalues);
  ikptr val = ffi_to_scheme_value_cast(unfix(rtype), rvalue, pcb);
  for(i=0; i<n; i++){
    free(avalues[i]);
  }
  free(avalues);
  free(rvalue);
  return val;
}

void hello_world(int n) {
  while(n > 0) {
    fprintf(stderr, "Hello World\n");
    n--;
  }
}

#else
ikptr ikrt_ffi_prep_cif() { return false_object; }
ikrt_ffi_call()           { return false_object; }
#endif





