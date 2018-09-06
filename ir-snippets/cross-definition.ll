 @.const.0 = private unnamed_addr constant float 10.000000, align 4
define i64 @main(i32 %ll.main.arg1){
 %ll.main.0 = call float @test1(i32 %ll.main.arg1)
 %ll.main.1 = fptosi float %ll.main.0 to i64
 ret i64 %ll.main.1
}
define float @test1(i32 %ll.test1.arg1){
 %ll.test1.0 = add i32 %ll.test1.arg1, %ll.test1.arg1
 %ll.test1.1 = sitofp i32 %ll.test1.0 to float
 %ll.test1.2.0 = load float, float* @.const.0
 %ll.test1.2.1 = fadd float %ll.test1.1, %ll.test1.2.0
 ret float %ll.test1.2.1
}
