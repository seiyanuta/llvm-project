# static int local1, local2; // Local Symbols.
# int global1;        // A coomon symmbol.
# char global2 = 123; // A extern symbol.
# int global3 = 456;  // A extern symbol.
# extern int extern1, extern2, extern3, extern4; // Undefined symbols.
# int func(void); // A undefined symbol.
#
# int sum() {
#   return global1 + global2 + local1 + local2 +
#     extern1 + extern2 + extern3 + extern4 + func();
# }

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
	subq	$16, %rsp
	movq	_extern4@GOTPCREL(%rip), %rax
	movq	_extern3@GOTPCREL(%rip), %rcx
	movq	_extern2@GOTPCREL(%rip), %rdx
	movq	_extern1@GOTPCREL(%rip), %rsi
	movq	_global1@GOTPCREL(%rip), %rdi
	movl	(%rdi), %r8d
	movsbl	_global2(%rip), %r9d
	addl	%r9d, %r8d
	addl	_local1(%rip), %r8d
	addl	_local2(%rip), %r8d
	addl	(%rsi), %r8d
	addl	(%rdx), %r8d
	addl	(%rcx), %r8d
	addl	(%rax), %r8d
	movl	%r8d, -4(%rbp)          ## 4-byte Spill
	callq	_func
	movl	-4(%rbp), %r8d          ## 4-byte Reload
	addl	%eax, %r8d
	movl	%r8d, %eax
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_global2                ## @global2
_global2:
	.byte	123                     ## 0x7b

	.globl	_global3                ## @global3
	.p2align	2
_global3:
	.long	456                     ## 0x1c8

	.comm	_global1,4,2            ## @global1
.zerofill __DATA,__bss,_local1,4,2      ## @local1
.zerofill __DATA,__bss,_local2,4,2      ## @local2

.subsections_via_symbols
