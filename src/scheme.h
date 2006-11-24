/* automatically generated, do not edit */
#ifndef SCHEME_H
#define  SCHEME_H
typedef char* ptr;
#define fx_shift 2
#define fx_mask 3
#define fx_tag 0
#define bool_f ((ptr)47)
#define bool_t ((ptr)63)
#define bool_mask 239
#define bool_tag 47
#define bool_shift 4
#define empty_list ((ptr)79)
#define wordsize 4
#define char_shift 8
#define char_tag 15
#define char_mask 255
#define pair_mask 7
#define pair_tag 1
#define disp_car 0
#define disp_cdr 4
#define pair_size 8
#define symbol_mask 7
#define symbol_tag 2
#define disp_symbol_string 0
#define disp_symbol_value 8
#define symbol_size 16
#define vector_tag 5
#define vector_mask 7
#define disp_vector_length 0
#define disp_vector_data 4
#define string_mask 7
#define string_tag 6
#define disp_string_length 0
#define disp_string_data 4
#define closure_mask 7
#define closure_tag 3
#define disp_closure_data 4
#define disp_closure_code 0
#define record_pmask 7
#define record_ptag 5
#define disp_record_data 4
#define disp_record_rtd 0
#define continuation_tag 31
#define disp_continuation_top 4
#define disp_continuation_size 8
#define disp_continuation_next 12
#define continuation_size 16
#define code_tag 47
#define disp_code_instrsize 4
#define disp_code_relocsize 8
#define disp_code_closuresize 12
#define disp_code_data 16
#define disp_frame_offset -13
#define disp_frame_size -17
#define object_alignment 8
#define align_shift 3
typedef struct {
   ptr system_stack;
   ptr stack_top;
   ptr stack_size;
   ptr frame_base;
   ptr frame_redline;
   ptr frame_pointer;
   ptr heap_base;
   ptr heap_size;
   ptr allocation_redline;
   ptr allocation_pointer;
   ptr roots;
   ptr string_base;
   ptr string_ap;
   ptr string_eap;
   ptr string_pages;
   ptr allocated_megs;
   ptr allocated_bytes;
   ptr reclaimed_megs;
   ptr reclaimed_bytes;
   ptr scheme_objects;
   ptr next_continuation;
   ptr prim_21;
   ptr prim_22;
   ptr prim_23;
   ptr prim_24;
   ptr prim_25;
   ptr prim_26;
   ptr prim_27;
   ptr prim_28;
   ptr prim_29;
   ptr prim_30;
   ptr prim_31;
   ptr prim_32;
   ptr prim_33;
   ptr prim_34;
   ptr prim_35;
   ptr prim_36;
   ptr prim_37;
   ptr prim_38;
   ptr prim_39;
   ptr prim_40;
   ptr prim_41;
   ptr prim_42;
   ptr prim_43;
   ptr prim_44;
   ptr prim_45;
   ptr prim_46;
   ptr prim_47;
   ptr prim_48;
   ptr prim_49;
   ptr prim_50;
   ptr prim_51;
   ptr prim_52;
   ptr prim_53;
   ptr prim_54;
   ptr prim_55;
   ptr prim_56;
   ptr prim_57;
   ptr prim_58;
   ptr prim_59;
   ptr prim_60;
   ptr prim_61;
   ptr prim_62;
   ptr prim_63;
   ptr prim_64;
   ptr prim_65;
   ptr prim_66;
   ptr prim_67;
   ptr prim_68;
   ptr prim_69;
   ptr prim_70;
   ptr prim_71;
   ptr prim_72;
   ptr prim_73;
   ptr prim_74;
   ptr prim_75;
   ptr prim_76;
   ptr prim_77;
   ptr prim_78;
   ptr prim_79;
   ptr prim_80;
   ptr prim_81;
   ptr prim_82;
   ptr prim_83;
   ptr prim_84;
   ptr prim_85;
   ptr prim_86;
   ptr prim_87;
   ptr prim_88;
   ptr prim_89;
   ptr prim_90;
   ptr prim_91;
   ptr prim_92;
   ptr prim_93;
   ptr prim_94;
   ptr prim_95;
   ptr prim_96;
   ptr prim_97;
   ptr prim_98;
   ptr prim_99;
   ptr prim_100;
   ptr prim_101;
   ptr prim_102;
   ptr prim_103;
   ptr prim_104;
   ptr prim_105;
   ptr prim_106;
   ptr prim_107;
   ptr prim_108;
   ptr prim_109;
   ptr prim_110;
   ptr prim_111;
   ptr prim_112;
   ptr prim_113;
   ptr prim_114;
   ptr prim_115;
   ptr prim_116;
   ptr prim_117;
   ptr prim_118;
   ptr prim_119;
   ptr prim_120;
   ptr prim_121;
   ptr prim_122;
   ptr prim_123;
   ptr prim_124;
   ptr prim_125;
   ptr prim_126;
   ptr prim_127;
   ptr prim_128;
   ptr prim_129;
   ptr prim_130;
   ptr prim_131;
   ptr prim_132;
   ptr prim_133;
   ptr prim_134;
   ptr prim_135;
   ptr prim_136;
   ptr prim_137;
   ptr prim_138;
   ptr prim_139;
   ptr prim_140;
   ptr prim_141;
   ptr prim_142;
   ptr prim_143;
   ptr prim_144;
   ptr prim_145;
   ptr prim_146;
   ptr prim_147;
   ptr prim_148;
   ptr prim_149;
   ptr prim_150;
   ptr prim_151;
   ptr prim_152;
   ptr prim_153;
   ptr prim_154;
   ptr prim_155;
   ptr prim_156;
   ptr prim_157;
   ptr prim_158;
   ptr prim_159;
   ptr prim_160;
   ptr prim_161;
   ptr prim_162;
   ptr prim_163;
   ptr prim_164;
   ptr prim_165;
   ptr prim_166;
   ptr prim_167;
   ptr prim_168;
   ptr prim_169;
   ptr prim_170;
   ptr prim_171;
   ptr prim_172;
   ptr prim_173;
   ptr prim_174;
   ptr prim_175;
   ptr prim_176;
   ptr prim_177;
   ptr prim_178;
   ptr prim_179;
   ptr prim_180;
   ptr prim_181;
   ptr prim_182;
   ptr prim_183;
   ptr prim_184;
   ptr prim_185;
   ptr prim_186;
   ptr prim_187;
   ptr prim_188;
   ptr prim_189;
   ptr prim_190;
   ptr prim_191;
   ptr prim_192;
   ptr prim_193;
   ptr prim_194;
   ptr prim_195;
   ptr prim_196;
   ptr prim_197;
   ptr prim_198;
   ptr prim_199;
   ptr prim_200;
   ptr prim_201;
   ptr prim_202;
   ptr prim_203;
   ptr prim_204;
   ptr prim_205;
   ptr prim_206;
   ptr prim_207;
   ptr prim_208;
   ptr prim_209;
   ptr prim_210;
   ptr prim_211;
   ptr prim_212;
   ptr prim_213;
   ptr prim_214;
   ptr prim_215;
   ptr prim_216;
   ptr prim_217;
   ptr prim_218;
   ptr prim_219;
   ptr prim_220;
   ptr prim_221;
   ptr prim_222;
   ptr prim_223;
   ptr prim_224;
   ptr prim_225;
   ptr prim_226;
   ptr prim_227;
   ptr prim_228;
   ptr prim_229;
   ptr prim_230;
   ptr prim_231;
   ptr prim_232;
   ptr prim_233;
   ptr prim_234;
   ptr prim_235;
   ptr prim_236;
   ptr prim_237;
   ptr prim_238;
   ptr prim_239;
   ptr prim_240;
   ptr prim_241;
   ptr scheme_objects_end;
} pcb_t;
ptr scheme_entry(pcb_t* pcb);
extern ptr scheme_main(pcb_t* pcb);
#endif /* SCHEME_H */
