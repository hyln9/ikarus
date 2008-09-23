
#include "ikarus-data.h"
#include "config.h"

#if ENABLE_LIBFFI
#include <ffi.h>
#include <stdlib.h>
#include <strings.h>

#undef DEBUG_FFI

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
  ffi_abi abi = FFI_DEFAULT_ABI;
  unsigned int nargs = unfix(ref(argstptr, off_vector_length));
  ffi_type** argtypes = alloc(sizeof(ffi_type*), nargs+1);
  int i;
  for(i=0; i<nargs; i++){
    ikptr argt = ref(argstptr, off_vector_data + i*wordsize);
    argtypes[i] = scheme_to_ffi_type_cast(unfix(argt));
  }
  argtypes[nargs] = NULL;
  ffi_type* rtype = scheme_to_ffi_type_cast(unfix(rtptr));
  ffi_status s = ffi_prep_cif(cif, abi, nargs, rtype, argtypes);
  if (s == FFI_OK) {
    ikptr r = ik_safe_alloc(pcb, pointer_size);
    ref(r, 0) = pointer_tag;
    ref(r, wordsize) = (ikptr)cif;
    return r + vector_tag;
  } else {
    return false_object;  
  }
}


        
static void
dump_stack(ikpcb* pcb, char* msg) {
  fprintf(stderr, "====================  %s\n", msg);
  ikptr frame_base = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
  ikptr p = frame_pointer;
  fprintf(stderr, "fp=0x%016lx   base=0x%016lx\n", frame_pointer, frame_base);
  while(p < frame_base) {
    fprintf(stderr, "*0x%016lx = 0x%016lx\n", p, ref(p, 0));
    p += wordsize;
  }
}


/* FIXME: handle stack overflow */
ikptr
ikrt_seal_scheme_stack(ikpcb* pcb) {
  #if 0
           |              |
           |              |
           |              |
           |              |
           +--------------+
           |   underflow  |  <--------- new frame pointer
           +--------------+
           | return point |  <--------- old frame pointer, new frame base
           +--------------+
           |      .       |
           |      .       |
           |      .       |
           |              |
           +--------------+
           |   underflow  |  <--------- old frame base 
           +--------------+
  #endif
  ikptr frame_base = pcb->frame_base;
  ikptr frame_pointer = pcb->frame_pointer;
#ifdef DEBUG_FFI
  dump_stack(pcb, "BEFORE SEALING");
  fprintf(stderr, "old base=0x%016lx  fp=0x%016lx\n", pcb->frame_base,
      pcb->frame_pointer);
#endif
  if ((frame_base - wordsize) != frame_pointer) {
    ikptr underflow_handler = ref(frame_base, -wordsize);
    cont* k = (cont*) pcb->next_k;
    cont* nk = (cont*) ik_unsafe_alloc(pcb, sizeof(cont));
    nk->tag = continuation_tag;
    nk->next = (ikptr) k;
    nk->top = frame_pointer;
#ifdef DEBUG_FFI
    fprintf(stderr, "rp=0x%016lx\n",
        ref(frame_pointer, 0));
#endif
    nk->size = frame_base - frame_pointer - wordsize;
#ifdef DEBUG_FFI
    fprintf(stderr, "frame size=%ld\n", nk->size);
#endif
    pcb->next_k = vector_tag + (ikptr)nk;
    pcb->frame_base = frame_pointer;
    pcb->frame_pointer = pcb->frame_base - wordsize;
#ifdef DEBUG_FFI
    fprintf(stderr, "new base=0x%016lx  fp=0x%016lx\n", pcb->frame_base,
        pcb->frame_pointer);
    fprintf(stderr, "uf=0x%016lx\n", underflow_handler);
#endif
    ref(pcb->frame_pointer, 0) = underflow_handler;
  } else {
#ifdef DEBUG_FFI
    fprintf(stderr, "already sealed\n");
#endif
  }
#ifdef DEBUG_FFI
  dump_stack(pcb, "AFTER SEALING");
#endif
  return void_object;
}

ikptr
ikrt_call_back(ikptr proc, ikpcb* pcb) {
  ikrt_seal_scheme_stack(pcb);

  ikptr old_k = pcb->next_k;
  pcb->next_k = 0;
  ikptr entry_point = ref(proc, off_closure_code);
  ikptr system_stack = pcb->system_stack;
#ifdef DEBUG_FFI
  fprintf(stderr, "system_stack = 0x%016lx\n", pcb->system_stack);
#endif
  ikptr code_ptr = entry_point - off_code_data;
  pcb->frame_pointer = pcb->frame_base;
  ikptr rv = ik_exec_code(pcb, code_ptr, 0, proc); 
#ifdef DEBUG_FFI
  fprintf(stderr, "system_stack = 0x%016lx\n", pcb->system_stack);
#endif
#ifdef DEBUG_FFI
  fprintf(stderr, "rv=0x%016lx\n", rv);
#endif
  pcb->next_k = old_k;
  pcb->frame_pointer = pcb->frame_base - wordsize;
#ifdef DEBUG_FFI
  fprintf(stderr, "rp=0x%016lx\n", ref(pcb->frame_pointer, 0));
#endif
  pcb->system_stack = system_stack;
  return rv;
}



ikptr
ikrt_ffi_call(ikptr data, ikptr argsvec, ikpcb* pcb)  {

  ikrt_seal_scheme_stack(pcb);
  ikptr old_k = pcb->next_k;
  pcb->next_k = 0;
  ikptr system_stack = pcb->system_stack;

  ikptr cifptr  = ref(data, off_vector_data + 0 * wordsize);
  ikptr funptr  = ref(data, off_vector_data + 1 * wordsize);
  ikptr typevec = ref(data, off_vector_data + 2 * wordsize);
  ikptr rtype   = ref(data, off_vector_data + 3 * wordsize);
  ffi_cif* cif = (ffi_cif*) ref(cifptr, off_pointer_data);
  void* fn = (void*) ref(funptr, off_pointer_data);
  unsigned int n = unfix(ref(argsvec, off_vector_length));
  void** avalues = alloc(sizeof(void*), n+1);
  int i;
  for(i=0; i<n; i++){
    ikptr t = ref(typevec, off_vector_data + i * wordsize);
    ikptr v = ref(argsvec, off_vector_data + i * wordsize);
    avalues[i] = scheme_to_ffi_value_cast(unfix(t), v);
  }
  avalues[n] = NULL;
  void* rvalue = alloc_room_for_type(unfix(rtype));;
  ffi_call(cif, fn, rvalue, avalues);
  ikptr val = ffi_to_scheme_value_cast(unfix(rtype), rvalue, pcb);
  for(i=0; i<n; i++){
    free(avalues[i]);
  }
#ifdef DEBUG_FFI
  fprintf(stderr, "DONE WITH CALL, RV=0x%016lx\n", (long)val);
#endif
  free(avalues);
  free(rvalue);
  pcb->frame_pointer = pcb->frame_base - wordsize;
  pcb->next_k = old_k;
  pcb->system_stack = system_stack;
  return val;
}


/*

ffi_status ffi_prep_cif (
  ffi_cif *cif, 
  ffi_abi abi,
  unsigned int nargs,
  ffi_type *rtype, 
  ffi_type **argtypes)

void *ffi_closure_alloc (size_t size, void **code)

void ffi_closure_free (void *writable)

ffi_status ffi_prep_closure_loc (
  ffi_closure *closure,
  ffi_cif *cif,
  void (*fun) (ffi_cif *cif, void *ret, void **args, void *user_data),
  void *user_data,
  void *codeloc)

*/

extern ikpcb* the_pcb;
static void 
generic_callback(ffi_cif *cif, void *ret, void **args, void *user_data){
  /* convert args according to cif to scheme values */
  /* call into scheme, get the return value */
  /* convert the return value to C */
  /* put the C return value in *ret */
  /* done */
  ikptr data = ((callback_locative*)user_data)->data;
  ikptr proc   = ref(data, off_vector_data + 1 * wordsize);
  ikptr argtypes_conv = ref(data, off_vector_data + 2 * wordsize);
  ikptr rtype_conv = ref(data, off_vector_data + 3 * wordsize);

  ikpcb* pcb = the_pcb;
  ikptr old_system_stack = pcb->system_stack; /* preserve */
  ikptr old_next_k = pcb->next_k;             /* preserve */
  pcb->next_k = 0;
  ikptr code_entry = ref(proc, off_closure_code);
  ikptr code_ptr = code_entry - off_code_data;
  ikptr frame_pointer = pcb->frame_pointer;
  ikptr frame_base = pcb->frame_base;
  /*
  if ((frame_base - wordsize) != frame_pointer) {
    fprintf(stderr, "ikarus internal error: INVALID FRAME LAYOUT 0x%016lx .. 0x%016lx\n", 
        frame_base, frame_pointer);
    exit(-1);
  }
  */
  pcb->frame_pointer = pcb->frame_base;
  ref(pcb->frame_pointer, -2*wordsize) = fix(*((int*)args[0]));
  ikptr rv = ik_exec_code(pcb, code_ptr, fix(-1), proc); 
#ifdef DEBUG_FFI
  fprintf(stderr, "and back with rv=0x%016lx!\n", rv);
#endif
  pcb->system_stack = old_system_stack;
  pcb->next_k = old_next_k;
  *((ikptr*)ret) = unfix(rv);
  return;
}

ikptr
ikrt_prepare_callback(ikptr data, ikpcb* pcb){
#if FFI_CLOSURES
  ikptr cifptr = ref(data, off_vector_data + 0 * wordsize);
  void* codeloc;
  ffi_closure* closure = ffi_closure_alloc(sizeof(ffi_closure), &codeloc);
  ffi_cif* cif = (ffi_cif*) ref(cifptr, off_pointer_data);
  
  callback_locative* loc = malloc(sizeof(callback_locative));
  if(!loc) {
    fprintf(stderr, "ERROR: ikarus malloc error\n");
    exit(-1);
  }
  
  ffi_status st = 
    ffi_prep_closure_loc(closure, cif, generic_callback, loc, codeloc);

  if (st != FFI_OK) {
    free(loc);
    return false_object;
  }

  loc->data = data;
  loc->next = pcb->callbacks;
  pcb->callbacks = loc;

  ikptr p = ik_safe_alloc(pcb, pointer_size);
  ref(p, 0) = pointer_tag;
  ref(p, wordsize) = (ikptr) codeloc;
  return p+vector_tag;
#else
  return false_object
#endif
}

int ho (int(*f)(int), int n) {
 // fprintf(stderr, "HO HO 0x%016lx!\n", (long)f);
  int n0 = f(n);
 // fprintf(stderr, "GOT N0\n");
  return n0 + f(n);
}


int ho2 (ikptr fptr, ikptr nptr) {
  int (*f)(int) =  (int(*)(int)) ref(fptr, off_pointer_data);
  int n = unfix(nptr);
 // fprintf(stderr, "HO2 HO2 0x%016lx!\n", (long)f);
  int n0 = f(n);
 // fprintf(stderr, "GOT N0\n");
  return n0 + f(n);
}




int cadd1 (int n) {
  return n+1;
}

void hello_world(int n) {
  while(n > 0) {
    fprintf(stderr, "Hello World\n");
    n--;
  }
}

#else
ikptr ikrt_ffi_prep_cif()    { return false_object; }
ikptr ikrt_ffi_call()              { return false_object; }
ikptr ikrt_prepare_callback() { return false_object; }
#endif





