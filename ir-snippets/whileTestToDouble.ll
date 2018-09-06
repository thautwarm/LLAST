 @.const.0 = private unnamed_addr constant i32 0, align 4
 @.const.1 = private unnamed_addr constant i32 1, align 4
 @.const.2 = private unnamed_addr constant i32 2, align 4
define i32 @whileTest(i32 %.whileTest.arg1){
 %.whileTest.0 = load i32, i32* @.const.0
 %.whileTest.1 = alloca i32, i32 %.whileTest.0 ,align 4
 %.whileTest.2.0 = load i32, i32* @.const.0
 %.whileTest.2.1 = alloca i32, i32 %.whileTest.2.0 ,align 4
.beginWhile:
 %.whileTest.2.2.0 = load i32, i32* %.whileTest.2.1
 %.whileTest.2.2.1 = icmp slt i32 %.whileTest.2.2.0, %.whileTest.arg1
 br i1 %.whileTest.2.2.1, label %.endWhile, label %.beginWhileBody
.beginWhileBody:
 %.whileTest.2.2.2 = load i32, i32* %.whileTest.2.1
 %.whileTest.2.2.3 = load i32, i32* @.const.1
 %.whileTest.2.2.4 = add i32 %.whileTest.2.2.2, %.whileTest.2.2.3
 store i32 %.whileTest.2.2.4, i32 * %.whileTest.2.1
 %.whileTest.2.2.5 = load i32, i32* %.whileTest.1
 %.whileTest.2.2.6 = load i32, i32* @.const.2
 %.whileTest.2.2.7 = add i32 %.whileTest.2.2.5, %.whileTest.2.2.6
 store i32 %.whileTest.2.2.7, i32 * %.whileTest.1
 br label %.beginWhile
.endWhile:
 %.whileTest.2.2.8 = load i32, i32* %.whileTest.1
 ret i32 %.whileTest.2.2.8
}
