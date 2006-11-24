
#include "ikarus.h"
#include <strings.h>
#include <string.h>

static ikoblist*
initialize_symbol_table(ikpcb* pcb){
  #define NUM_OF_BUCKETS 4096 /* power of 2 */
  ikoblist* st = ik_malloc(sizeof(ikoblist));
  st->number_of_buckets = NUM_OF_BUCKETS;
  int size = NUM_OF_BUCKETS * sizeof(ikbucket*);
  st->buckets = ik_mmap(size);
  bzero(st->buckets, size);
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
  ikoblist* st = pcb->oblist;
  int n = st->number_of_buckets;
  ikbucket** bs = st->buckets;
  ikp ac = null_object;
  int i;
  for(i=0; i<n; i++){
    ikbucket* b = bs[i];
    while(b){
      ikp p = ik_alloc(pcb, pair_size) + pair_tag;
      ref(p, off_car) = b->val;
      ref(p, off_cdr) = ac;
      ac = p;
      b = b->next;
    }
  }
  return ac;
}

ikp ik_intern_string(ikp str, ikpcb* pcb){
  //fprintf(stderr, "0x%08x: intern %s => ", (int)pcb, string_data(str));
  ikoblist* st = pcb->oblist;
  if(st == 0){
    st = initialize_symbol_table(pcb);
  }
  int h = compute_hash(str);
  int idx = h & (st->number_of_buckets - 1);
  ikbucket* b = st->buckets[idx];
  while(b){
//    if(b->key == (ikp) h){
      ikp sym = b->val;
      ikp sym_str = ref(sym, off_symbol_string);
      if(strings_eqp(sym_str, str)){
        //fprintf(stderr, "SAME %s\n", string_data(str));
        return sym;
      }
//    }
    b = b->next;
  }
  ikp sym = ik_make_symbol(str, pcb);
  b = ik_malloc(sizeof(ikbucket));
  b->key = (ikp)h;
  b->val = sym;
  b->next = st->buckets[idx];
  st->buckets[idx] = b;
  //fprintf(stderr, "NEW\n");
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
