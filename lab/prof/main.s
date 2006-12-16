	.text
.globl _foo
_foo:
LFB3:
	leaq	1(%rdi), %rax
	ret
LFE3:
	.cstring
LC0:
	.ascii "sizeof(long long int)=%ld\12\0"
	.text
.globl _main
_main:
LFB20:
	subq	$8, %rsp
LCFI0:
	movl	$8, %edx
	leaq	LC0(%rip), %rsi
	movq	___stderrp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	xorl	%eax, %eax
	call	_fprintf
	movl	$-1, %edi
	call	_exit
LFE20:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0x0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
	.globl _foo.eh
_foo.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB3-.
	.set L$set$2,LFE3-LFB3
	.quad L$set$2
	.byte	0x0
	.align 3
LEFDE1:
	.globl _main.eh
_main.eh:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB20-.
	.set L$set$4,LFE20-LFB20
	.quad L$set$4
	.byte	0x0
	.byte	0x4
	.set L$set$5,LCFI0-LFB20
	.long L$set$5
	.byte	0xe
	.byte	0x10
	.align 3
LEFDE3:
	.subsections_via_symbols
