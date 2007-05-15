
#include "ikarus.h"
#include <strings.h>
#include <string.h>
#include <stdlib.h>

static ikp
make_symbol_table(ikpcb* pcb){
  #define NUM_OF_BUCKETS 4096 /* power of 2 */
  int size = align_to_next_page(disp_vector_data + NUM_OF_BUCKETS * wordsize);
  ikp st = ik_mmap_ptr(size, 0, pcb) + vector_tag;
  bzero(st-vector_tag, size);
  ref(st, off_vector_length) = fix(NUM_OF_BUCKETS);
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

#if 0
static ikp 
ik_make_symbol(ikp str, ikp ustr, ikpcb* pcb){
  ikp sym = ik_alloc(pcb, symbol_size) + symbol_tag;
  ref(sym, off_symbol_string)  = str;
  ref(sym, off_symbol_ustring) = ustr;
  ref(sym, off_symbol_value)   = unbound_object;
  ref(sym, off_symbol_plist)   = null_object;
  ref(sym, off_symbol_system_value) = str;
  ref(sym, off_symbol_code) = 0;
  ref(sym, off_symbol_errcode) = 0;
  ref(sym, off_symbol_unused) = 0;
  return sym;
}
#endif

static ikp 
ik_make_symbol(ikp str, ikp ustr, ikpcb* pcb){
  ikp sym = ik_alloc(pcb, symbol_record_size) + record_tag;
  ref(sym, -record_tag) = symbol_record_tag;
  ref(sym, off_symbol_record_string)  = str;
  ref(sym, off_symbol_record_ustring) = ustr;
  ref(sym, off_symbol_record_value)   = unbound_object;
  ref(sym, off_symbol_record_proc)    = str;
  ref(sym, off_symbol_record_plist)   = null_object;
  return sym;
}



static ikp
intern_string(ikp str, ikp st, ikpcb* pcb){
  int h = compute_hash(str);
  int idx = h & (unfix(ref(st, off_vector_length)) - 1);
  ikp bckt = ref(st, off_vector_data + idx*wordsize);
  ikp b = bckt;
  while(b){
    ikp sym = ref(b, off_car);
    ikp sym_str = ref(sym, off_symbol_record_string);
    if(strings_eqp(sym_str, str)){
      return sym;
    }
    b = ref(b, off_cdr);
  }
  ikp sym = ik_make_symbol(str, false_object,  pcb);
  b = ik_alloc(pcb, pair_size) + pair_tag;
  ref(b, off_car) = sym;
  ref(b, off_cdr) = bckt;
  ref(st, off_vector_data + idx*wordsize) = b;
  pcb->dirty_vector[page_index(st+off_vector_data+idx*wordsize)] = -1;
  return sym;
}

static ikp
intern_unique_string(ikp str, ikp ustr, ikp st, ikpcb* pcb){
  int h = compute_hash(ustr);
  int idx = h & (unfix(ref(st, off_vector_length)) - 1);
  ikp bckt = ref(st, off_vector_data + idx*wordsize);
  ikp b = bckt;
  while(b){
    ikp sym = ref(b, off_car);
    ikp sym_ustr = ref(sym, off_symbol_record_ustring);
    if(strings_eqp(sym_ustr, ustr)){
      return sym;
    }
    b = ref(b, off_cdr);
  }
  ikp sym = ik_make_symbol(str, ustr, pcb);
  b = ik_alloc(pcb, pair_size) + pair_tag;
  ref(b, off_car) = sym;
  ref(b, off_cdr) = bckt;
  ref(st, off_vector_data + idx*wordsize) = b;
  pcb->dirty_vector[page_index(st+off_vector_data+idx*wordsize)] = -1;
  return sym;
}

ikp
ikrt_intern_gensym(ikp sym, ikpcb* pcb){
  ikp st = pcb->gensym_table;
  if(st == 0){
    st = make_symbol_table(pcb);
    pcb->gensym_table = st;
  }
  ikp ustr = ref(sym, off_symbol_record_ustring);
  int h = compute_hash(ustr);
  int idx = h & (unfix(ref(st, off_vector_length)) - 1);
  ikp bckt = ref(st, off_vector_data + idx*wordsize);
  ikp b = bckt;
  while(b){
    ikp sym = ref(b, off_car);
    ikp sym_ustr = ref(sym, off_symbol_record_ustring);
    if(strings_eqp(sym_ustr, ustr)){
      return false_object;
    }
    b = ref(b, off_cdr);
  }
  b = ik_alloc(pcb, pair_size) + pair_tag;
  ref(b, off_car) = sym;
  ref(b, off_cdr) = bckt;
  ref(st, off_vector_data + idx*wordsize) = b;
  pcb->dirty_vector[page_index(st+off_vector_data+idx*wordsize)] = -1;
  return true_object;
}




ikp 
ikrt_string_to_symbol(ikp str, ikpcb* pcb){
  ikp st = pcb->symbol_table;
  if(st == 0){
    st = make_symbol_table(pcb);
    pcb->symbol_table = st;
  }
  return intern_string(str, st, pcb);
}

ikp 
ik_intern_string(ikp str, ikpcb* pcb){
  return ikrt_string_to_symbol(str, pcb);
}

ikp 
ikrt_strings_to_gensym(ikp str, ikp ustr, ikpcb* pcb){
  ikp st = pcb->gensym_table;
  if(st == 0){
    st = make_symbol_table(pcb);
    pcb->gensym_table = st;
  }
  return intern_unique_string(str, ustr, st, pcb);
}



ikp
ik_cstring_to_symbol(char* str, ikpcb* pcb){
  int n = strlen(str);
  int size = n + disp_string_data + 1;
  ikp s = ik_alloc(pcb, align(size)) + string_tag;
  ref(s, off_string_length) = fix(n);
  memcpy(s+off_string_data, str, n+1);
  ikp sym = ikrt_string_to_symbol(s, pcb);
  return sym;
}
