 @.const.0 = private unnamed_addr constant float 10.000000, align 4
define i64 @test2(i32 %.test2.arg1){
 %.test2.0 = call float @test1(i32 %.test2.arg1)
 %.test2.1 = fptosi float %.test2.0 to i64
 ret i64 %.test2.1
}
define float @test1(i32 %.test1.arg1){
 %.test1.0 = add i32 %.test1.arg1, %.test1.arg1
 %.test1.1 = sitofp i32 %.test1.0 to float
 %.test1.var$2.0 = load float, float* @.const.0
 %.test1.var$2.1 = fadd float %.test1.1, %.test1.var$2.0
 ret float %.test1.var$2.1
}
