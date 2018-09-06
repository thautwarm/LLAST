 @.const.0 = private unnamed_addr constant i1 1, align 1
 @.const.1 = private unnamed_addr constant i1 0, align 1
 @.const.2 = private unnamed_addr constant i32 123, align 4
 @.const.3 = private unnamed_addr constant i32 456, align 4
 @.const.4 = private unnamed_addr constant i32 111, align 4
 @.const.5 = private unnamed_addr constant i32 222, align 4
define i32 @test3(i32 %ll.test3.arg1){
 %ll.test3.0 = alloca i32, align 4
 %ll.test3.1.0 = alloca i32, align 4
 %ll.test3.1.1.0 = load i1, i1* @.const.0
 %ll.test3.1.1.1 = load i1, i1* @.const.1
 %ll.test3.1.1.2 = icmp eq i1 %ll.test3.1.1.0, %ll.test3.1.1.1
 br i1 %ll.test3.1.1.2, label %ll.test3.1.1.5, label %ll.test3.1.1.3
ll.test3.1.1.3:
 %ll.test3.1.1.4 = load i32, i32* @.const.2
 store i32 %ll.test3.1.1.4, i32* %ll.test3.1.0
 br label %ll.test3.1.1.7
ll.test3.1.1.5:
 %ll.test3.1.1.6 = load i32, i32* @.const.3
 store i32 %ll.test3.1.1.6, i32* %ll.test3.1.0
ll.test3.1.1.7:
 %ll.test3.1.1.8 = load i32, i32* %ll.test3.1.0
 %ll.test3.1.2 = load i32, i32* @.const.3
 %ll.test3.1.3 = icmp eq i32 %ll.test3.1.1.8, %ll.test3.1.2
 br i1 %ll.test3.1.3, label %ll.test3.1.6, label %ll.test3.1.4
ll.test3.1.4:
 %ll.test3.1.5 = load i32, i32* @.const.4
 store i32 %ll.test3.1.5, i32* %ll.test3.0
 br label %ll.test3.1.8
ll.test3.1.6:
 %ll.test3.1.7 = load i32, i32* @.const.5
 store i32 %ll.test3.1.7, i32* %ll.test3.0
ll.test3.1.8:
 %ll.test3.1.9 = load i32, i32* %ll.test3.0
 ret i32 %ll.test3.1.9
}
