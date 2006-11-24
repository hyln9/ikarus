
#ifndef IKARUS_H
#define IKARUS_H

#include <stdio.h>

extern int total_allocated_pages;
extern int total_malloced;
extern int hash_table_count;


static int 
inthash(int key) {
  key += ~(key << 15);
  key ^=  (key >> 10);
  key +=  (key << 3);
  key ^=  (key >> 6);
  key += ~(key << 11);
  key ^=  (key >> 16);
  return key;
  return inthash(key);
}



typedef unsigned char* ikp;
void ik_error(ikp args);

typedef struct ikpages{
  ikp base;
  int size;
  struct ikpages* next;
} ikpages;

typedef struct ikdl{ /* double-link */
  struct ikdl* prev;
  struct ikdl* next;
} ikdl;


#define ikcode_dead    ((unsigned int) 0x0000)
#define ikcode_live    ((unsigned int) 0x0001)
#define ikcode_queued  ((unsigned int) 0x0002 | ikcode_live)
#define ikcode_scanned ((unsigned int) 0x0004 | ikcode_live)

typedef struct ikcodes{ /* a doubly-linked list of code objects */
  ikdl dl;
  ikp code_object;
  unsigned int attr;
  ikp base;
  int size;
} ikcodes;

typedef struct ikhashtables{
  ikp ht;
  struct ikhashtables* next;
} ikhashtables;

typedef struct{
  ikcodes* link;
  int padding; /* for 8-byte alignment */
} ikcode_preheader;

typedef struct ikbucket{
  ikp key;
  ikp val;
  struct ikbucket* next;
} ikbucket;

typedef struct{
  int number_of_buckets;
  ikbucket** buckets;
} ikoblist;

typedef struct {
  /* the first locations may be accessed by some     */
  /* compiled code to perform overflow/underflow ops */
  ikp   allocation_pointer;           /* offset =  0 */
  ikp   allocation_redline;           /* offset =  4 */
  ikp   frame_pointer;                /* offset =  8 */
  ikp   frame_base;                   /* offset = 12 */
  ikp   frame_redline;                /* offset = 16 */
  ikp   next_k;                       /* offset = 20 */
  void* system_stack;                 /* offset = 24 */
  /* the rest are not used by any scheme code        */
  /* they only support the runtime system (gc, etc.) */
  ikp   heap_base; 
  int   heap_size;
  ikp   stack_base;
  int   stack_size;
  ikpages* heap_pages;
  ikdl codes;
  ikhashtables* hash_tables;
  ikoblist*   oblist;
} ikpcb;



void* ik_malloc(int);
void ik_free(void*, int);

void* ik_mmap(int);
void ik_munmap(void*, int);
ikpcb* ik_make_pcb();
void ik_delete_pcb(ikpcb*);

void ik_fasl_load(ikpcb* pcb, char* filename);

ikp ik_exec_code(ikpcb* pcb, ikp code_ptr);
void ik_print(ikp x);
void ik_fprint(FILE*, ikp x);

ikp ik_intern_string(ikp, ikpcb*);

ikp ik_cstring_to_symbol(char*, ikpcb*);

ikp ik_asm_enter(ikpcb*, ikp code_object, ikp arg);
ikp ik_asm_reenter(ikpcb*, ikp code_object, ikp val);
ikp ik_underflow_handler(ikpcb*);
ikp ik_alloc(ikpcb* pcb, int size);
#include "ikarus-data.h"
#define wordsize 4
#define pagesize 4096
#define ik_eof_p(x) ((x) == ik_eof_object)

#endif
