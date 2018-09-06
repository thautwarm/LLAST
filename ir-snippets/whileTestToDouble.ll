 @.const.0 = private unnamed_addr constant i32 0, align 4
 @.const.1 = private unnamed_addr constant i32 1, align 4
 @.const.2 = private unnamed_addr constant i32 2, align 4
define i32 @whileTest(i32 %ll.whileTest.arg1){
 %ll.whileTest.0 = load i32, i32* @.const.0
 %ll.whileTest.1 = alloca i32, i32 %ll.whileTest.0 ,align 4
 %ll.whileTest.2.0 = load i32, i32* @.const.0
 %ll.whileTest.2.1 = alloca i32, i32 %ll.whileTest.2.0 ,align 4
ll.whileTest.2.2.0:
 %ll.whileTest.2.2.1 = load i32, i32* %ll.whileTest.2.1
 %ll.whileTest.2.2.2 = icmp slt i32 %ll.whileTest.2.2.1, %ll.whileTest.arg1
 br i1 %ll.whileTest.2.2.2, label %ll.whileTest.2.2.10, label %ll.whileTest.2.2.3
ll.whileTest.2.2.3:
 %ll.whileTest.2.2.4 = load i32, i32* %ll.whileTest.2.1
 %ll.whileTest.2.2.5 = load i32, i32* @.const.1
 %ll.whileTest.2.2.6 = add i32 %ll.whileTest.2.2.4, %ll.whileTest.2.2.5
 store i32 %ll.whileTest.2.2.6, i32* %ll.whileTest.2.1
 %ll.whileTest.2.2.7 = load i32, i32* %ll.whileTest.1
 %ll.whileTest.2.2.8 = load i32, i32* @.const.2
 %ll.whileTest.2.2.9 = add i32 %ll.whileTest.2.2.7, %ll.whileTest.2.2.8
 store i32 %ll.whileTest.2.2.9, i32* %ll.whileTest.1
 br label %ll.whileTest.2.2.0
ll.whileTest.2.2.10:
 %ll.whileTest.2.2.11 = load i32, i32* %ll.whileTest.1
 ret i32 %ll.whileTest.2.2.11
}
