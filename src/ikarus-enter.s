#  Ikarus Scheme -- A compiler for R6RS Scheme.
#  Copyright (C) 2006,2007  Abdulaziz Ghuloum
#  
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 3 as
#  published by the Free Software Foundation.
#  
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.



.text
.globl ik_asm_enter
.globl _ik_asm_enter
.globl ik_underflow_handler
.globl ik_foreign_call
.globl _ik_foreign_call
.globl ik_asm_reenter
.globl _ik_asm_reenter
.align 8
ik_asm_enter:
_ik_asm_enter:
  # ignored value is the third arg 12(%esp)
  # code is the second arg  8(%esp)
  # pcb is the first arg    4(%esp)
  # return point is at      0(%esp)
  movl %esi, -4(%esp)    #  preserve
  movl %ebp, -8(%esp)    #  preserve
  movl 4(%esp), %esi   
  movl 0(%esi), %ebp     # allocation pointer is at 0(pcb)
  movl %esp, %eax
  subl $16, %esp         # 24 for alignment
set_stack:
  movl %esp, 24(%esi)    # save esp in pcb->system_stack
  movl 8(%esi), %esp     # load scheme stack from pcb->frame_pinter
  jmp L_call
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .long L_multivalue_underflow
  .byte 0
  .byte 0
L_call:
  call *8(%eax)         # goooooooo
  # now we're back
ik_underflow_handler:
  movl %eax, -8(%esp)   # store the return value
  movl $-4, %eax        # set rvcount = 1
L_do_underflow:
  movl %esp, 8(%esi)    # store scheme stack in pcb->frame_pointer
  movl %ebp, 0(%esi)    # store allocation pointer
  movl 24(%esi), %esp   # restore system stack
  addl $16, %esp        # 24 for alignment (>= 16)
  movl -4(%esp), %esi   # restore callee-save registers
  movl -8(%esp), %ebp   #
  ret                   # back to C, which handled the underflow
L_multivalue_underflow:
  addl $4, %esp
  jmp L_do_underflow

.align 8
ik_asm_reenter:
_ik_asm_reenter:
  # argc is at 12(%esp)
  # scheme stack is third arg   8(%esp)
  # pcb is the first arg        4(%esp)
  # return point is at          0(%esp)
  movl 12(%esp), %eax
  movl  8(%esp), %ebx
  movl %esi, -4(%esp)
  movl %ebp, -8(%esp)
  movl 4(%esp), %esi   
  movl 0(%esi), %ebp     # allocation pointer is at 0(pcb)
  subl $16, %esp         # 24 for alignment
  movl %esp, 24(%esi)    # save esp in pcb->system_stack
  movl %ebx, %esp        # load scheme stack from second arg
  cmpl $-4, %eax
  jne L_multi_reentry
  movl -4(%esp), %eax
  ret
L_multi_reentry:
  movl 0(%esp), %ebx
  jmp *-9(%ebx)


.align 8
ik_foreign_call:
_ik_foreign_call:
  movl %esp, 8(%esi)     # (movl fpr (pcb-ref 'frame-pointer))
  movl %ebp, 0(%esi)     # (movl apr (pcb-ref 'allocation-pointer))
  movl %esp, %ebx        # (movl fpr ebx)
  movl 24(%esi), %esp    # (movl (pcb-ref 'system-stack) esp)
  # %esp is the system stack, %eax is the index to the last arg,
  # %esi is the pcb.
  # Now, the value of %esp is 16-byte aligned
  # we always push %esi (4 bytes) and do a call (4 bytes),
  # 0 args require 6 (2) pushes => argc=   0 (0000): %esp +=  -8
  # 1 args require 5 (1) pushes => argc=  -4 (1100): %esp +=  -4
  # 2 args require 4 (0) pushes => argc=  -8 (1000): %esp +=   0
  # 3 args require 3 (3) pushes => argc= -12 (0100): %esp += -12
  movl %eax, %ecx
  andl $15, %ecx
check_ecx:
  cmpl $8, %ecx
  je L_zero
  cmpl $12, %ecx
  je L_one
  cmpl $0, %ecx
  je L_two
  pushl $0
L_two:
  pushl $0
L_one:
  pushl $0
L_zero:
  pushl %esi             # (pushl pcr)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  je L_set               # (je (label Lset))
L_loop:                  # (label Lloop)
  movl (%ebx,%eax), %ecx # (movl (mem ebx eax) ecx)
  pushl %ecx             # (pushl ecx)
  addl $4, %eax          # (addl (int 4) eax)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  jne L_loop             # (jne (label Lloop))
L_set:                   # (label Lset)
  call *%edi             # (call cpr)
  movl 8(%esi), %esp     # (movl (pcb-ref 'frame-pointer) fpr)
  movl 0(%esi), %ebp     # (movl (pcb-ref 'allocation-pointer) apr)
  ret                    # (ret)))

#ifdef __CYGWIN__
	.section .drectve

	.ascii " -export:ik_collect"
	.ascii " -export:ik_collect_vararg"
	.ascii " -export:ik_dump_dirty_vector"
	.ascii " -export:ik_dump_metatable"
	.ascii " -export:ik_error"
	.ascii " -export:ik_stack_overflow"
	.ascii " -export:ik_system"
	.ascii " -export:ik_uuid"
	.ascii " -export:ikrt_bignum_shift_left"
	.ascii " -export:ikrt_bignum_shift_right"
	.ascii " -export:ikrt_bignum_to_bytevector"
	.ascii " -export:ikrt_bnbncomp"
	.ascii " -export:ikrt_bnbndivrem"
	.ascii " -export:ikrt_bnbnlogand"
	.ascii " -export:ikrt_bnbnminus"
	.ascii " -export:ikrt_bnbnmult"
	.ascii " -export:ikrt_bnbnplus"
	.ascii " -export:ikrt_bnfxdivrem"
	.ascii " -export:ikrt_bnfxminus"
	.ascii " -export:ikrt_bvftime"
	.ascii " -export:ikrt_bytes_allocated"
	.ascii " -export:ikrt_bytes_allocated_major"
	.ascii " -export:ikrt_bytevector_to_flonum"
	.ascii " -export:ikrt_close_file"
	.ascii " -export:ikrt_delete_file"
	.ascii " -export:ikrt_environ"
	.ascii " -export:ikrt_even_bn"
	.ascii " -export:ikrt_exit"
	.ascii " -export:ikrt_file_exists"
	.ascii " -export:ikrt_fixnum_shift_left"
	.ascii " -export:ikrt_fixnum_to_flonum"
	.ascii " -export:ikrt_fl_acos"
	.ascii " -export:ikrt_fl_asin"
	.ascii " -export:ikrt_fl_atan"
	.ascii " -export:ikrt_fl_cos"
	.ascii " -export:ikrt_fl_equal"
	.ascii " -export:ikrt_fl_exp"
	.ascii " -export:ikrt_fl_invert"
	.ascii " -export:ikrt_fl_less"
	.ascii " -export:ikrt_fl_less_or_equal"
	.ascii " -export:ikrt_fl_log"
	.ascii " -export:ikrt_fl_round"
	.ascii " -export:ikrt_fl_sin"
	.ascii " -export:ikrt_fl_sqrt"
	.ascii " -export:ikrt_fl_tan"
	.ascii " -export:ikrt_flfl_expt"
	.ascii " -export:ikrt_fork"
	.ascii " -export:ikrt_fx_acos"
	.ascii " -export:ikrt_fx_asin"
	.ascii " -export:ikrt_fx_atan"
	.ascii " -export:ikrt_fx_cos"
	.ascii " -export:ikrt_fx_log"
	.ascii " -export:ikrt_fx_sin"
	.ascii " -export:ikrt_fx_sqrt"
	.ascii " -export:ikrt_fx_tan"
	.ascii " -export:ikrt_fxbnlogand"
	.ascii " -export:ikrt_fxbnminus"
	.ascii " -export:ikrt_fxbnmult"
	.ascii " -export:ikrt_fxbnplus"
	.ascii " -export:ikrt_fxfxminus"
	.ascii " -export:ikrt_fxfxmult"
	.ascii " -export:ikrt_fxfxplus"
	.ascii " -export:ikrt_fxrandom"
	.ascii " -export:ikrt_getenv"
	.ascii " -export:ikrt_intern_gensym"
	.ascii " -export:ikrt_is_weak_pair"
	.ascii " -export:ikrt_make_code"
	.ascii " -export:ikrt_open_input_file"
	.ascii " -export:ikrt_open_output_file"
	.ascii " -export:ikrt_positive_bn"
	.ascii " -export:ikrt_read"
	.ascii " -export:ikrt_register_guardian_pair"
	.ascii " -export:ikrt_set_code_annotation"
	.ascii " -export:ikrt_set_code_reloc_vector"
	.ascii " -export:ikrt_setenv"
	.ascii " -export:ikrt_stats_now"
	.ascii " -export:ikrt_string_to_symbol"
	.ascii " -export:ikrt_strings_to_gensym"
	.ascii " -export:ikrt_waitpid"
	.ascii " -export:ikrt_weak_cons"
	.ascii " -export:ikrt_write_file"
	.ascii " -export:ik_foreign_call"
#endif
