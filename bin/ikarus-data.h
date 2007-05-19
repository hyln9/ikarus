#ifndef IKARUS_DATA_H
#define IKARUS_DATA_H

#define IK_FASL_HEADER "#@IK01"
#define IK_FASL_HEADER_LEN (strlen(IK_FASL_HEADER))
#define IK_FASL_CODE_HEADER_SIZE 12

#define code_pri_tag vector_tag
#define code_tag                  ((ikp)0x2F)
#define disp_code_code_size 4
#define disp_code_reloc_vector 8
#define disp_code_freevars 12
#define disp_code_data 16
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
#define disp_bytevector_data   4
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
#define port_size       32

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

#endif
