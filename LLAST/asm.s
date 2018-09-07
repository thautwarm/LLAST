	.text
	.file	"whileTestToDouble.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	movl	.L.const.0(%rip), %eax
	movl	%eax, -4(%rsp)
	movl	%eax, -8(%rsp)
	movl	.L.const.1(%rip), %eax
	cmpl	%eax, -8(%rsp)
	jge	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %ll.main.2.2.4
                                        # =>This Inner Loop Header: Depth=1
	movl	.L.const.2(%rip), %ecx
	addl	%ecx, -8(%rsp)
	movl	.L.const.3(%rip), %ecx
	addl	%ecx, -4(%rsp)
	cmpl	%eax, -8(%rsp)
	jl	.LBB0_2
.LBB0_3:                                # %ll.main.2.2.11
	movl	-4(%rsp), %eax
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L.const.0,@object      # @.const.0
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2
.L.const.0:
	.long	0                       # 0x0
	.size	.L.const.0, 4

	.type	.L.const.1,@object      # @.const.1
	.p2align	2
.L.const.1:
	.long	123                     # 0x7b
	.size	.L.const.1, 4

	.type	.L.const.2,@object      # @.const.2
	.p2align	2
.L.const.2:
	.long	1                       # 0x1
	.size	.L.const.2, 4

	.type	.L.const.3,@object      # @.const.3
	.p2align	2
.L.const.3:
	.long	2                       # 0x2
	.size	.L.const.3, 4


	.section	".note.GNU-stack","",@progbits
