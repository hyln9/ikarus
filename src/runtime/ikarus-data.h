#ifndef IKARUS_DATA_H
#define IKARUS_DATA_H

#define IK_FASL_HEADER "#@IK01"
#define IK_FASL_HEADER_LEN (strlen(IK_FASL_HEADER))
#define IK_FASL_CODE_HEADER_SIZE 12

#define IK_CODE_PRI_TAG              5
#define code_pri_tag vector_tag
#define IK_CODE_SEC_TAG           ((ikp)0x2F)
#define code_tag                  ((ikp)0x2F)
#define IK_DISP_CODE_CODE_SIZE       4
#define disp_code_code_size 4
#define IK_DISP_CODE_RELOC_SIZE      8
#define disp_code_reloc_size 8
#define IK_DISP_CODE_CLOSURE_SIZE   12
#define disp_code_closure_size 12
#define IK_DISP_CODE_DATA           16
#define disp_code_data 16
#define off_code_data (disp_code_data - code_pri_tag)

#define IK_OFF_CODE_DATA (IK_DISP_CODE_DATA - IK_CODE_PRI_TAG)

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
#define unbound_object    ((ikp)0x6F)
#define IK_CHAR_TAG       0x0F
#define IK_CHAR_MASK      0xFF
#define IK_CHAR_SHIFT        8
#define IK_CHAR_VAL(x)    (((int)(x)) >> IK_CHAR_SHIFT)
#define byte_to_scheme_char(x) ((ikp)(((x) << IK_CHAR_SHIFT) | IK_CHAR_TAG))
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
#define IK_HEAP_EXT_SIZE  (16 * 4096)
#define IK_PAIRP(x)   (IK_PTAG(x) == IK_PAIR_TAG)
#define IK_CHARP(x)   (IK_MASK(x,IK_CHAR_MASK) == IK_CHAR_TAG)
#define IK_STRING_TAG     6
#define string_tag        6
#define disp_string_length    0
#define disp_string_data      4
#define off_string_length (disp_string_length - string_tag)
#define off_string_data (disp_string_data - string_tag)

#define string_data(x)  ((char*)((x) + off_string_data))


#define vector_tag 5
#define disp_vector_length 0
#define disp_vector_data 4
#define off_vector_data (disp_vector_data - vector_tag)
#define off_vector_length (disp_vector_length - vector_tag)


#define symbol_tag    2
#define disp_symbol_string        0
#define disp_symbol_ustring       4
#define disp_symbol_value         8
#define disp_symbol_plist        12 
#define disp_symbol_system_value 16
#define disp_symbol_system_plist 20
#define symbol_size  24
#define off_symbol_string (disp_symbol_string - symbol_tag)
#define off_symbol_ustring (disp_symbol_ustring - symbol_tag)
#define off_symbol_value (disp_symbol_value - symbol_tag)
#define off_symbol_plist (disp_symbol_plist - symbol_tag)
#define off_symbol_system_value (disp_symbol_system_value - symbol_tag)
#define off_symbol_system_plist (disp_symbol_system_plist - symbol_tag)

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
#define disp_rtd_rtd     0
#define disp_rtd_name    4
#define disp_rtd_length  8
#define disp_rtd_field  12
#define rtd_size 16

#define off_rtd_rtd    (disp_rtd_rtd    - rtd_tag) 
#define off_rtd_name   (disp_rtd_name   - rtd_tag) 
#define off_rtd_length (disp_rtd_length - rtd_tag) 
#define off_rtd_field  (disp_rtd_field  - rtd_tag) 

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

#define htable_tag  ((ikp) 0x3F)
#define disp_htable_count  4
#define disp_htable_size   8
#define disp_htable_mem   12
#define htable_size       16
#define off_htable_count (disp_htable_count - vector_tag) 
#define off_htable_size  (disp_htable_size  - vector_tag) 
#define off_htable_mem   (disp_htable_mem   - vector_tag) 

#endif
