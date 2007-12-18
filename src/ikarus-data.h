/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007  Abdulaziz Ghuloum
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *  
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



#ifndef IKARUS_H
#define IKARUS_H

#include <stdio.h>
#include <sys/resource.h>

extern int total_allocated_pages;
extern int total_malloced;
extern int hash_table_count;

#define cardsize 512
#define cards_per_page 8

#define most_bytes_in_minor 0x10000000

#define old_gen_mask       0x00000007
#define new_gen_mask       0x00000008
#define gen_mask           0x0000000F
#define new_gen_tag        0x00000008
#define meta_dirty_mask    0x000000F0
#define type_mask          0x00000F00
#define scannable_mask     0x0000F000
#define dealloc_mask       0x000F0000
#define large_object_mask  0x00100000
#define meta_dirty_shift 4

#define hole_type       0x00000000
#define mainheap_type   0x00000100
#define mainstack_type  0x00000200
#define pointers_type   0x00000300
#define dat_type        0x00000400
#define code_type       0x00000500
#define weak_pairs_type 0x00000600
#define symbols_type    0x00000700

#define scannable_tag   0x00001000
#define unscannable_tag 0x00000000

#define dealloc_tag_un  0x00010000
#define dealloc_tag_at  0x00020000
#define retain_tag      0x00000000

#define large_object_tag   0x00100000

#define hole_mt         (hole_type       | unscannable_tag | retain_tag)
#define mainheap_mt     (mainheap_type   | unscannable_tag | retain_tag)
#define mainstack_mt    (mainstack_type  | unscannable_tag | retain_tag)
#define pointers_mt     (pointers_type   | scannable_tag   | dealloc_tag_un)
#define symbols_mt      (symbols_type    | scannable_tag   | dealloc_tag_un)
#define data_mt         (dat_type        | unscannable_tag | dealloc_tag_un)
#define code_mt         (code_type       | scannable_tag   | dealloc_tag_un)
#define weak_pairs_mt   (weak_pairs_type | scannable_tag   | dealloc_tag_un)


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



#define wordsize 4
#define wordshift 2
#define pagesize 4096
#define generation_count 5  /* generations 0 (nursery), 1, 2, 3, 4 */

typedef unsigned char* ikp;
void ik_error(ikp args);

typedef struct ikpage{
  ikp base;
  struct ikpage* next;
} ikpage;

typedef struct ikpages{
  ikp base;
  int size;
  struct ikpages* next;
} ikpages;

typedef struct ikdl{ /* double-link */
  struct ikdl* prev;
  struct ikdl* next;
} ikdl;


#define ik_ptr_page_size \
  ((pagesize - sizeof(int) - sizeof(struct ik_ptr_page*))/sizeof(ikp))

typedef struct ik_ptr_page{
  int count;
  struct ik_ptr_page* next;
  ikp ptr[ik_ptr_page_size];
} ik_ptr_page;
 

typedef struct ikpcb{
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
  ikp   arg_list;                     /* offset = 32 */
  int   engine_counter;               /* offset = 36 */
  int   interrupted;                  /* offset = 40 */
  ikp   base_rtd;                     /* offset = 44 */
  ikp   collect_key;                  /* offset = 48 */
  /* the rest are not used by any scheme code        */
  /* they only support the runtime system (gc, etc.) */
  ikp* root0;
  ikp* root1;
  unsigned int* segment_vector; 
  ikp weak_pairs_ap;
  ikp weak_pairs_ep;
  ikp   heap_base; 
  int   heap_size;
  ikpages* heap_pages;
  ikpage* cached_pages; /* pages cached so that we don't map/unmap */
  ikpage* uncached_pages; /* ikpages cached so that we don't malloc/free */
  ikp cached_pages_base;
  int cached_pages_size;
  ikp   stack_base;
  int   stack_size;
  ikp   symbol_table;
  ikp   gensym_table;
  ik_ptr_page* guardians[generation_count];
  ik_ptr_page* guardians_dropped[generation_count];
  unsigned int* dirty_vector_base;
  unsigned int* segment_vector_base;
  unsigned char* memory_base;
  unsigned char* memory_end;
  int collection_id;
  int allocation_count_minor;
  int allocation_count_major;
  struct timeval collect_utime;
  struct timeval collect_stime;
  struct timeval collect_rtime; 
} ikpcb;

ikpcb* ik_collect(int req, ikpcb* pcb);
void ikarus_usage_short(void);

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

ikp ikrt_string_to_symbol(ikp, ikpcb*);
ikp ikrt_strings_to_gensym(ikp, ikp,  ikpcb*);

ikp ik_cstring_to_symbol(char*, ikpcb*);

ikp ik_asm_enter(ikpcb*, ikp code_object, ikp arg);
ikp ik_asm_reenter(ikpcb*, ikp code_object, ikp val);
ikp ik_underflow_handler(ikpcb*);
ikp ik_unsafe_alloc(ikpcb* pcb, int size);
ikp ik_safe_alloc(ikpcb* pcb, int size);


#define IK_FASL_HEADER "#@IK01"
#define IK_FASL_HEADER_LEN (strlen(IK_FASL_HEADER))
#define IK_FASL_CODE_HEADER_SIZE 12

#define code_pri_tag vector_tag
#define code_tag                  ((ikp)0x2F)
#define disp_code_code_size 4
#define disp_code_reloc_vector 8
#define disp_code_freevars 12
#define disp_code_annotation 16
#define disp_code_data 24
#define off_code_annotation (disp_code_annotation - code_pri_tag)
#define off_code_data (disp_code_data - code_pri_tag)
#define off_code_reloc_vector (disp_code_reloc_vector - code_pri_tag)


#define IK_ALIGN_SHIFT 3
#define align_shift 3
#define IK_ALIGN_SIZE  8
#define align_size 8

#define IK_ALIGN(n) \
        ((((n) + IK_ALIGN_SIZE - 1) >>  IK_ALIGN_SHIFT) << IK_ALIGN_SHIFT)

#define align(n) \
        ((((n) + align_size - 1) >>  align_shift) << align_shift)

#define IK_FX_SHIFT 2
#define IK_FX_MASK  3
#define fx_shift 2
#define fx_mask 3
#define unfix(x) (((int)(x)) >> fx_shift)
#define fix(x)   ((ikp)((x) << fx_shift))
#define is_fixnum(x) ((((int)(x)) & fx_mask) == 0)

#define IK_FIXNUMP(x) \
  ((((int)(x)) & IK_FX_MASK) == 0)

#define REF(x,n) \
  (((ikp*)(((char*)(x)) + ((int)(n))))[0])

#define ref(x,n) \
  (((ikp*)(((char*)(x)) + ((int)(n))))[0])

#define IK_MASK(x,m) (((int)(x)) & ((int)(m)))
#define IK_PTAG(x) (((int)(x)) & 7)
#define tagof(x) (((int)(x)) & 7)
#define IK_STAG(x) REF(x, -IK_PTAG(x))

#define immediate_tag 7
 
#define IK_UNFIX(x) (((int)(x)) >> IK_FX_SHIFT)
#define IK_FIX(x)   ((ikp)((x) << IK_FX_SHIFT))
#define fix(x)   ((ikp)((x) << fx_shift))

#define IK_CODE_P(x) \
  ((IK_PTAG(x) == IK_CODE_PRI_TAG) && (IK_STAG(x) == IK_CODE_SEC_TAG))

#define IK_FALSE_OBJECT   ((ikp)0x2F)
#define IK_TRUE_OBJECT    ((ikp)0x3F)

#define false_object      ((ikp)0x2F)
#define true_object       ((ikp)0x3F)

#define IK_NULL_OBJECT    ((ikp)0x4F)

#define null_object       ((ikp)0x4F)
#define void_object       ((ikp)0x7F)
#define bwp_object        ((ikp)0x8F)

#define unbound_object    ((ikp)0x6F)
#define IK_CHAR_TAG       0x0F
#define IK_CHAR_MASK      0xFF
#define IK_CHAR_SHIFT        8
#define IK_CHAR_VAL(x)    (((int)(x)) >> IK_CHAR_SHIFT)
#define int_to_scheme_char(x) ((ikp)(((x) << IK_CHAR_SHIFT) | IK_CHAR_TAG))
#define IK_PAIR_SIZE     8
#define pair_size 8
#define pair_tag 1
#define disp_car 0
#define disp_cdr 4
#define off_car (disp_car - pair_tag)
#define off_cdr (disp_cdr - pair_tag)
#define IK_PAIR_TAG      1
#define IK_DISP_CAR      0
#define IK_DISP_CDR      4
#define IK_OFF_CAR    (IK_DISP_CAR - IK_PAIR_TAG)
#define IK_OFF_CDR    (IK_DISP_CDR - IK_PAIR_TAG)
#define IK_HEAP_EXT_SIZE  (32 * 4096)
#define IK_PAIRP(x)   (IK_PTAG(x) == IK_PAIR_TAG)
#define IK_CHARP(x)   (IK_MASK(x,IK_CHAR_MASK) == IK_CHAR_TAG)
#define IK_STRING_TAG     6
#define string_tag        6
#define disp_string_length    0
#define disp_string_data      4
#define off_string_length (disp_string_length - string_tag)
#define off_string_data (disp_string_data - string_tag)

//#define string_data(x)  ((char*)((x) + off_string_data))
//#define string_set(x,i,c) 
//  ((((unsigned char*)(x)) + off_string_data + (int)(i))[0] = 
//   (((int)(c)) >> IK_CHAR_SHIFT))
#define string_set(x,i,c) \
  (((ikp*)(((ikp)(x)) + off_string_data))[i] = ((ikp)(c)))
#define integer_to_char(x) ((ikp)((((int)(x)) << IK_CHAR_SHIFT) + IK_CHAR_TAG))
#define string_char_size 4

#define vector_tag 5
#define disp_vector_length 0
#define disp_vector_data 4
#define off_vector_data (disp_vector_data - vector_tag)
#define off_vector_length (disp_vector_length - vector_tag)


#if 0
#define symbol_tag    2
#define disp_symbol_string        0
#define disp_symbol_ustring       4
#define disp_symbol_value         8
#define disp_symbol_plist        12 
#define disp_symbol_system_value 16
#define disp_symbol_code 20
#define disp_symbol_errcode 24
#define disp_symbol_unused 28
#define symbol_size  32
#define off_symbol_string (disp_symbol_string - symbol_tag)
#define off_symbol_ustring (disp_symbol_ustring - symbol_tag)
#define off_symbol_value (disp_symbol_value - symbol_tag)
#define off_symbol_plist (disp_symbol_plist - symbol_tag)
#define off_symbol_system_value (disp_symbol_system_value - symbol_tag)
#define off_symbol_code (disp_symbol_code - symbol_tag)
#define off_symbol_errcode (disp_symbol_errcode - symbol_tag)
#define off_symbol_unused (disp_symbol_unused - symbol_tag)
#endif

#define bytevector_tag 2
#define disp_bytevector_length 0
#define disp_bytevector_data   8
#define off_bytevector_length (disp_bytevector_length - bytevector_tag)
#define off_bytevector_data (disp_bytevector_data - bytevector_tag)

#define symbol_record_tag ((ikp) 0x5F)
#define disp_symbol_record_string   4
#define disp_symbol_record_ustring  8
#define disp_symbol_record_value   12
#define disp_symbol_record_proc    16
#define disp_symbol_record_plist   20
#define symbol_record_size         24
#define off_symbol_record_string  (disp_symbol_record_string  - record_tag) 
#define off_symbol_record_ustring (disp_symbol_record_ustring - record_tag) 
#define off_symbol_record_value   (disp_symbol_record_value   - record_tag) 
#define off_symbol_record_proc    (disp_symbol_record_proc    - record_tag) 
#define off_symbol_record_plist   (disp_symbol_record_plist   - record_tag) 


#define closure_tag  3
#define closure_mask 7
#define disp_closure_code 0
#define disp_closure_data 4
#define off_closure_code (disp_closure_code - closure_tag)
#define off_closure_data (disp_closure_data - closure_tag)

#define is_closure(x) ((((int)(x)) & closure_mask) == closure_tag)


#define record_tag vector_tag
#define disp_record_rtd 0
#define disp_record_data 4
#define off_record_rtd  (disp_record_rtd  - record_tag)
#define off_record_data (disp_record_data - record_tag)

#define rtd_tag record_tag
#define disp_rtd_rtd      0
#define disp_rtd_name     4
#define disp_rtd_length   8
#define disp_rtd_fields  12
#define disp_rtd_printer 16
#define disp_rtd_symbol  20
#define rtd_size 24

#define off_rtd_rtd     (disp_rtd_rtd     - rtd_tag) 
#define off_rtd_name    (disp_rtd_name    - rtd_tag) 
#define off_rtd_length  (disp_rtd_length  - rtd_tag) 
#define off_rtd_fields  (disp_rtd_fields  - rtd_tag) 
#define off_rtd_printer (disp_rtd_printer - rtd_tag) 
#define off_rtd_symbol  (disp_rtd_symbol  - rtd_tag) 

#define continuation_tag      ((ikp)0x1F)
#define disp_continuation_top   4
#define disp_continuation_size  8
#define disp_continuation_next 12
#define continuation_size 16

#define off_continuation_top   (disp_continuation_top  - vector_tag) 
#define off_continuation_size  (disp_continuation_size - vector_tag)
#define off_continuation_next  (disp_continuation_next - vector_tag)

#define pagesize   4096
#define pageshift    12
#define align_to_next_page(x) \
  (((pagesize - 1 + (unsigned int)(x)) >> pageshift) << pageshift)
#define align_to_prev_page(x) \
  ((((unsigned int)(x)) >> pageshift) << pageshift)

#define disp_frame_size -17

#define port_tag      0x3F
#define port_mask     0x3F
#define port_size       56

#define disp_tcbucket_tconc        0
#define disp_tcbucket_key          4
#define disp_tcbucket_val          8
#define disp_tcbucket_next        12
#define tcbucket_size             16
#define off_tcbucket_tconc       (disp_tcbucket_tconc - vector_tag)
#define off_tcbucket_key         (disp_tcbucket_key   - vector_tag)
#define off_tcbucket_val         (disp_tcbucket_val   - vector_tag)
#define off_tcbucket_next        (disp_tcbucket_next  - vector_tag)


#define bignum_mask       0x7
#define bignum_tag        0x3
#define bignum_sign_mask  0x8
#define bignum_sign_shift   3
#define bignum_length_shift 4
#define disp_bignum_data wordsize
#define off_bignum_data (disp_bignum_data - vector_tag)

#define flonum_tag  ((ikp)0x17)
#define flonum_size         16
#define disp_flonum_data     8
#define off_flonum_data (disp_flonum_data - vector_tag)
#define flonum_data(x) (*((double*)(((ikp)(x))+off_flonum_data)))

#define ratnum_tag  ((ikp) 0x27)
#define ratnum_size        16
#define disp_ratnum_num     4
#define disp_ratnum_den     8
#define disp_ratnum_unused 12

#define ik_eof_p(x) ((x) == ik_eof_object)
#define page_index(x) (((unsigned int)(x)) >> pageshift)

#endif
