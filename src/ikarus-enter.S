#  Ikarus Scheme -- A compiler for R6RS Scheme.
#  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
.globl ik_foreign_call
.globl _ik_foreign_call
.globl ik_asm_reenter
.globl _ik_asm_reenter


#if __x86_64__

####################################################################
# 64-bit

.align 8
ik_asm_enter:
_ik_asm_enter:
# c parameters come in registers:
#     %rdi, %rsi, %rdx, %rcx, %r8 and %r9
# return value registers are %rax and %rdi
# callee-save registers:
#     %rbp, %rbx, %r12, r13, r14, %r15 are callee-save 


  # First, save all callee-save registers
  mov %rbp,  -8(%rsp)      # preserve
  mov %rbx,  -16(%rsp)     # preserve
  mov %r12,  -24(%rsp)     # preserve
  mov %r13,  -32(%rsp)     # preserve
  mov %r14,  -40(%rsp)     # preserve
  mov %r15,  -48(%rsp)     # preserve

  # code is the second arg, or  %rsi
  # pcb is the first arg, or    %rdi
  # return point is at       0(%rsp)

  mov %rsi, %rax     # move code pointer to %rax
  mov %rdi, %rsi     # move pcb into pcb-register (%rsi)
  mov 0(%rsi), %rbp  # allocation pointer is at 0(pcb)
  sub $64, %rsp         # 64 for alignment
  mov %rsp, 48(%rsi)    # save esp in pcb->system_stack
  mov 16(%rsi), %rsp    # load scheme stack from pcb->frame_pinter
  jmp L_call
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
  .byte 0
L_multivalue_label: # FIXME
  .long 0           # 2 longs 
  .long 0           # for return address
  .byte 0
  .byte 0
L_call:
  call *%rax         # goooooooo
  # now we're back
ik_underflow_handler:
  mov %rax, -16(%rsp)   # store the return value
  mov $-8, %rax         # set rvcount = 1
L_do_underflow:
  mov %rsp, 16(%rsi)    # store scheme stack in pcb->frame_pointer
  mov %rbp, 0(%rsi)     # store allocation pointer
  mov 48(%rsi), %rsp    # restore system stack
  add $64, %rsp         # 64 for alignment

  # restore callee-save registers
  mov -8(%rsp) ,  %rbp     # restore
  mov -16(%rsp),  %rbx     # restore
  mov -24(%rsp),  %r12     # restore
  mov -32(%rsp),  %r13     # restore
  mov -40(%rsp),  %r14     # restore
  mov -48(%rsp),  %r15     # restore
  ret      # back to C, which handled the underflow
L_multivalue_underflow:
  add $8, %rsp
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
  push $0
L_two:
  push $0
L_one:
  push $0
L_zero:
  push %rsi             # (pushl pcr)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  je L_set               # (je (label Lset))
L_loop:                  # (label Lloop)
  movl (%ebx,%eax), %ecx # (movl (mem ebx eax) ecx)
  push %rcx             # (pushl ecx)
  addl $4, %eax          # (addl (int 4) eax)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  jne L_loop             # (jne (label Lloop))
L_set:                   # (label Lset)
  call *%rdi             # (call cpr)
  movl 8(%esi), %esp     # (movl (pcb-ref 'frame-pointer) fpr)
  movl 0(%esi), %ebp     # (movl (pcb-ref 'allocation-pointer) apr)
  ret                    # (ret)))

#else  

####################################################################
# 32-bit

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
  .long  L_multivalue_underflow
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
  push $0
L_two:
  push $0
L_one:
  push $0
L_zero:
  push %esi             # (pushl pcr)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  je L_set               # (je (label Lset))
L_loop:                  # (label Lloop)
  movl (%ebx,%eax), %ecx # (movl (mem ebx eax) ecx)
  push %ecx             # (pushl ecx)
  addl $4, %eax          # (addl (int 4) eax)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  jne L_loop             # (jne (label Lloop))
L_set:                   # (label Lset)
  call *%edi             # (call cpr)
  movl 8(%esi), %esp     # (movl (pcb-ref 'frame-pointer) fpr)
  movl 0(%esi), %ebp     # (movl (pcb-ref 'allocation-pointer) apr)
  ret                    # (ret)))

#endif 

