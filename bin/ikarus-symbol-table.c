
#include "ikarus.h"
#include <strings.h>
#include <string.h>
#include <stdlib.h>

static ikp
initialize_symbol_table(ikpcb* pcb){
  #define NUM_OF_BUCKETS 4096 /* power of 2 */
  int size = align_to_next_page(disp_vector_data + NUM_OF_BUCKETS * wordsize);
  ikp st = ik_mmap_ptr(size, 0, pcb) + vector_tag;
  bzero(st-vector_tag, size);
  ref(st, off_vector_length) = fix(NUM_OF_BUCKETS);
  pcb->oblist = st;
  return st;
}

static int 
compute_hash(ikp str){
  int len = unfix(ref(str, off_string_length));
  char* data = (char*) str + off_string_data;
  int h = len;
  char* last = data + len;
  while(data < last){
    char c = *data;
    h = h + c;
    h = h + (h << 10);
    h = h ^ (h >> 6);
    data++;
  }
  h = h + (h << 3);
  h = h ^ (h >> 11);
  h = h + (h << 15);
  return h;
}

static int strings_eqp(ikp str1, ikp str2){
  ikp len = ref(str1, off_string_length);
  if(len == ref(str2, off_string_length)){
    return
      (memcmp(str1+off_string_data, str2+off_string_data, unfix(len)) == 0);
  }
  return 0;
}

static ikp ik_make_symbol(ikp str, ikpcb* pcb){
  ikp sym = ik_alloc(pcb, symbol_size) + symbol_tag;
  ref(sym, off_symbol_string)  = str;
  ref(sym, off_symbol_ustring) = false_object;
  ref(sym, off_symbol_value)   = unbound_object;
  ref(sym, off_symbol_plist)   = null_object;
  ref(sym, off_symbol_system_value) = str;
  ref(sym, off_symbol_system_plist) = null_object;
  return sym;
}

ikp ik_oblist(ikpcb* pcb){
  fprintf(stderr, "oblist dead!\n");
  exit(-1);
}

ikp ik_intern_string(ikp str, ikpcb* pcb){
  ikp st = pcb->oblist;
  if(st == 0){
    st = initialize_symbol_table(pcb);
  }
  int h = compute_hash(str);
  int idx = h & (unfix(ref(st, off_vector_length)) - 1);
  ikp bckt = ref(st, off_vector_data + idx*wordsize);
  ikp b = bckt;
  while(b){
    ikp sym = ref(b, off_car);
    ikp sym_str = ref(sym, off_symbol_string);
    if(strings_eqp(sym_str, str)){
      return sym;
    }
    b = ref(b, off_cdr);
  }
  ikp sym = ik_make_symbol(str, pcb);
  b = ik_alloc(pcb, pair_size) + pair_tag;
  ref(b, off_car) = sym;
  ref(b, off_cdr) = bckt;
  ref(st, off_vector_data + idx*wordsize) = b;
  pcb->dirty_vector[page_index(st+off_vector_data+idx*wordsize)] = -1;
  return sym;
}

ikp
ik_cstring_to_symbol(char* str, ikpcb* pcb){
  int n = strlen(str);
  int size = n + disp_string_data + 1;
  ikp s = ik_alloc(pcb, align(size)) + string_tag;
  ref(s, off_string_length) = fix(n);
  memcpy(s+off_string_data, str, n+1);
  ikp sym = ik_intern_string(s, pcb);
  return sym;
}
