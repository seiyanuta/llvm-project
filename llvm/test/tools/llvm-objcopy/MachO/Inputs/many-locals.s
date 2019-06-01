	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 14
	.globl	_foo                    ## -- Begin function foo
	.p2align	4, 0x90
_foo:                                   ## @foo
	.cfi_startproc
## %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	_a(%rip), %eax
	movl	%eax, -4(%rbp)          ## 4-byte Spill
	movb	$0, %al
	callq	_func
	movl	-4(%rbp), %ecx          ## 4-byte Reload
	addl	%eax, %ecx
	movl	%ecx, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
.zerofill __DATA,__bss,_a,4,2           ## @a

.subsections_via_symbols
