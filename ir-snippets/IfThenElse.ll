 @.const.0 = private unnamed_addr constant i1 1, align 1
 @.const.1 = private unnamed_addr constant i32 123, align 4
 @.const.2 = private unnamed_addr constant i32 456, align 4
 @.const.3 = private unnamed_addr constant i32 111, align 4
 @.const.4 = private unnamed_addr constant i32 222, align 4
define i32 @ifelsetest(i1 %.ifelsetest.arg1){
 %.ifelsetest.0 = alloca i32, align 4
 %.ifelsetest.1.0 = alloca i32, align 4
 %.ifelsetest.1.1.0 = load i1, i1* @.const.0
 %.ifelsetest.1.1.1 = icmp eq i1 %.ifelsetest.1.1.0, %.ifelsetest.arg1
 br i1 %.ifelsetest.1.1.1, label %.cond.elselabel, label %.cond.thenlabel
.cond.thenlabel:
 %.ifelsetest.1.1.2 = load i32, i32* @.const.1
 store i32 %.ifelsetest.1.1.2, i32 * %.ifelsetest.1.0
 br label %.cond.endif
.cond.elselabel:
 %.ifelsetest.1.1.3 = load i32, i32* @.const.2
 store i32 %.ifelsetest.1.1.3, i32 * %.ifelsetest.1.0
.cond.endif:
 %.ifelsetest.1.1.4 = load i32, i32* %.ifelsetest.1.0
 %.ifelsetest.1.2 = load i32, i32* @.const.2
 %.ifelsetest.1.3 = icmp eq i32 %.ifelsetest.1.1.4, %.ifelsetest.1.2
 br i1 %.ifelsetest.1.3, label %.elselabel, label %.thenlabel
.thenlabel:
 %.ifelsetest.1.4 = load i32, i32* @.const.3
 store i32 %.ifelsetest.1.4, i32 * %.ifelsetest.0
 br label %.endif
.elselabel:
 %.ifelsetest.1.5 = load i32, i32* @.const.4
 store i32 %.ifelsetest.1.5, i32 * %.ifelsetest.0
.endif:
 %.ifelsetest.1.6 = load i32, i32* %.ifelsetest.0
 ret i32 %.ifelsetest.1.6
}
