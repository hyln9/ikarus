
#ifndef IKARUS_H
#define IKARUS_H

#include <stdio.h>
#include <sys/resource.h>

extern int total_allocated_pages;
extern int total_malloced;
extern int hash_table_count;

#define cardsize 512
#define cards_per_page 8

#define old_gen_mask    0x00000007
#define new_gen_mask    0x00000008
#define gen_mask        0x0000000F
#define new_gen_tag     0x00000008
#define meta_dirty_mask 0x000000F0
#define type_mask       0x00000F00
#define scannable_mask  0x0000F000
#define dealloc_mask    0x000F0000
#define meta_dirty_shift 4

#define hole_type       0x00000000
#define mainheap_type   0x00000100
#define mainstack_type  0x00000200
#define pointers_type   0x00000300
#define data_type       0x00000400
#define code_type       0x00000500
#define weak_pairs_type 0x00000600

#define scannable_tag   0x00001000
#define unscannable_tag 0x00000000

#define dealloc_tag     0x00010000
#define retain_tag      0x00000000

#define hole_mt         (hole_type       | unscannable_tag | retain_tag)
#define mainheap_mt     (mainheap_type   | unscannable_tag | retain_tag)
#define mainstack_mt    (mainstack_type  | unscannable_tag | retain_tag)
#define pointers_mt     (pointers_type   | scannable_tag   | dealloc_tag)
#define data_mt         (data_type       | unscannable_tag | dealloc_tag)
#define code_mt         (code_type       | scannable_tag   | dealloc_tag)
#define weak_pairs_mt   (weak_pairs_type | scannable_tag   | dealloc_tag)


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
  unsigned int* dirty_vector;         /* offset = 28 */

  /* the rest are not used by any scheme code        */
  /* they only support the runtime system (gc, etc.) */
  unsigned int* segment_vector; 
  ikp weak_pairs_ap;
  ikp weak_pairs_ep;
  ikp   heap_base; 
  int   heap_size;
  ikpages* heap_pages;
  ikp   stack_base;
  int   stack_size;
  ikp   oblist;
  unsigned int* dirty_vector_base;
  unsigned int* segment_vector_base;
  unsigned char* memory_base;
  unsigned char* memory_end;
  int collection_id;
  struct timeval collect_utime;
  struct timeval collect_stime;
  
} ikpcb;



void* ik_malloc(int);
void ik_free(void*, int);

void* ik_mmap(int);
void* ik_mmap_typed(int size, unsigned int type, ikpcb*);
void* ik_mmap_ptr(int size, int gen, ikpcb*);
void* ik_mmap_data(int size, int gen, ikpcb*);
void* ik_mmap_code(int size, int gen, ikpcb*);
void* ik_mmap_mixed(int size, ikpcb*);
void ik_munmap(void*, int);
void ik_munmap_from_segment(unsigned char*, int, ikpcb*);
ikpcb* ik_make_pcb();
void ik_delete_pcb(ikpcb*);
void ik_free_symbol_table(ikpcb* pcb);

void ik_fasl_load(ikpcb* pcb, char* filename);
void ik_relocate_code(ikp);

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
#define wordshift 2
#define pagesize 4096
#define ik_eof_p(x) ((x) == ik_eof_object)
#define page_index(x) (((unsigned int)(x)) >> pageshift)

#endif