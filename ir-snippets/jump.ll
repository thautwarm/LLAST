 @.const.0 = private unnamed_addr constant i32 10, align 4
 @.const.1 = private unnamed_addr constant i32 15, align 4
define i32 @test3(i32 %.test3.arg1){
 %.test3.0 = alloca i32, align 4
 %.test3.result$1.0 = load i32, i32* @.const.0
 %.test3.result$1.1 = icmp eq i32 %.test3.result$1.0, %.test3.arg1
 br i1 %.test3.result$1.1, label %falselabel, label %truelabel
truelabel:
 br label %tag
falselabel:
 %.test3.result$1.2 = load i32, i32* @.const.0
 store i32 %.test3.result$1.2, i32 * %.test3.0
 %.test3.result$1.3 = load i32, i32* %.test3.0
 ret i32 %.test3.result$1.3
tag:
 %.test3.2 = load i32, i32* @.const.1
 ret i32 %.test3.2
}
