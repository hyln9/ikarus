
.text
.globl ik_asm_enter
.globl ik_underflow_handler
.globl ik_foreign_call
.globl ik_asm_reenter
.align 8
ik_asm_enter:
  # ignored value is the third arg 12(%esp)
  # code is the second arg  8(%esp)
  # pcb is the first arg    4(%esp)
  # return point is at      0(%esp)
  movl %esi, -4(%esp)    #  preserve
  movl %ebp, -8(%esp)    #  preserve
  movl 4(%esp), %esi   
  movl 0(%esi), %ebp     # allocation pointer is at 0(pcb)
  movl %esp, %eax
  subl $16, %esp
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
  addl $16, %esp
  movl -4(%esp), %esi   # restore callee-save registers
  movl -8(%esp), %ebp   #
  ret                   # back to C, which handled the underflow
L_multivalue_underflow:
  addl $4, %esp
  jmp L_do_underflow

.align 8
ik_asm_reenter:
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
  subl $16, %esp
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
  movl %esp, 8(%esi)     # (movl fpr (pcb-ref 'frame-pointer))
  movl %ebp, 0(%esi)     # (movl apr (pcb-ref 'allocation-pointer))
  movl %esp, %ebx        # (movl fpr ebx)
  movl 24(%esi), %esp    # (movl (pcb-ref 'system-stack) esp)
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


