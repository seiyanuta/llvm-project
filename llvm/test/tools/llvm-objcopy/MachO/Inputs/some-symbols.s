	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 14
	.globl	_sum                    ## -- Begin function sum
	.p2align	4, 0x90
_sum:                                   ## @sum
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	_extern4@GOTPCREL(%rip), %rax
	movq	_extern3@GOTPCREL(%rip), %rcx
	movq	_extern2@GOTPCREL(%rip), %rdx
	movq	_extern1@GOTPCREL(%rip), %rsi
	movq	_global2@GOTPCREL(%rip), %rdi
	movq	_global1@GOTPCREL(%rip), %r8
	movl	(%r8), %r9d
	addl	(%rdi), %r9d
	addl	_global3(%rip), %r9d
	addl	_static1(%rip), %r9d
	addl	_static2(%rip), %r9d
	addl	(%rsi), %r9d
	addl	(%rdx), %r9d
	addl	(%rcx), %r9d
	addl	(%rax), %r9d
	movl	%r9d, %eax
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_global3                ## @global3
	.p2align	2
_global3:
	.long	123                     ## 0x7b

	.comm	_global1,4,2            ## @global1
	.comm	_global2,4,2            ## @global2
.zerofill __DATA,__bss,_static1,4,2     ## @static1
.zerofill __DATA,__bss,_static2,4,2     ## @static2

.subsections_via_symbols
