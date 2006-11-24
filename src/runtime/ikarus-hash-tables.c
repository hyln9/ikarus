
#include "ikarus.h"
#include <strings.h>

/* from http://www.concentric.net/~Ttwang/tech/inthash.htm */

int 
inthash(int key) {
  key += ~(key << 15);
  key ^=  (key >> 10);
  key +=  (key << 3);
  key ^=  (key >> 6);
  key += ~(key << 11);
  key ^=  (key >> 16);
  return key;
}

ikp 
ik_get_hash_table(ikp ht, ikp k, ikp def, ikpcb* pcb){
  ikp size = ref(ht, off_htable_size);
  if(size == 0){
    return def;
  }
  ikbucket** table = (ikbucket**) ref(ht, off_htable_mem);
  int idx = inthash((int)k) & (unfix(size)-1);
  ikbucket* p = table[idx];
  while(p){
    if(p->key == k){
      return p->val;
    } else {
      p = p->next;
    }
  }
  return def;
}

int hash_table_count = 0;

static void
initialize_hash_table(ikp ht, ikpcb* pcb){
  hash_table_count++;
  ikp mem = ik_mmap(pagesize);
  bzero(mem, pagesize);
  ref(ht, off_htable_size)  = (ikp) pagesize;
  ref(ht, off_htable_count) = 0;
  ref(ht, off_htable_mem)   = mem;
  ikhashtables* p = ik_malloc(sizeof(ikhashtables));
  p->ht = ht;
  p->next = pcb->hash_tables;
  pcb->hash_tables = p;
}


ikp 
ik_put_hash_table(ikp ht, ikp k, ikp v, ikpcb* pcb){
  ikp size = ref(ht, off_htable_size);
  if(size == 0){
    initialize_hash_table(ht, pcb);
    size = ref(ht, off_htable_size);
  }
  ikbucket** table = (ikbucket**) ref(ht, off_htable_mem);
  int idx = inthash((int)k) & (unfix(size)-1);
  ikbucket* bucket = table[idx];
  ikbucket* p = bucket;
  while(p){
    if(p->key == k){
      p->val = v;
      return void_object;
    } else {
      p = p->next;
    }
  }
  p = ik_malloc(sizeof(ikbucket));
  p->key = k;
  p->val = v;
  p->next = bucket;
  table[idx] = p;
  ref(ht, off_htable_count) = 
    fix(unfix(ref(ht, off_htable_count)) + 1);
  return void_object;
}

