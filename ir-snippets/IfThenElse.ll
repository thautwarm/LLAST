 @.const.0 = private unnamed_addr constant i1 1, align 1
 @.const.1 = private unnamed_addr constant i1 0, align 1
 @.const.2 = private unnamed_addr constant i32 123, align 4
 @.const.3 = private unnamed_addr constant i32 456, align 4
 @.const.4 = private unnamed_addr constant i32 111, align 4
 @.const.5 = private unnamed_addr constant i32 222, align 4
define i32 @main(i32 %ll.main.arg1){
 %ll.main.0 = alloca i32, align 4
 %ll.main.1.0 = alloca i32, align 4
 %ll.main.1.1.0 = load i1, i1* @.const.0
 %ll.main.1.1.1 = load i1, i1* @.const.1
 %ll.main.1.1.2 = icmp eq i1 %ll.main.1.1.0, %ll.main.1.1.1
 br i1 %ll.main.1.1.2, label %ll.main.1.1.3, label %ll.main.1.1.5
ll.main.1.1.3:
 %ll.main.1.1.4 = load i32, i32* @.const.2
 store i32 %ll.main.1.1.4, i32* %ll.main.1.0
 br label %ll.main.1.1.7
ll.main.1.1.5:
 %ll.main.1.1.6 = load i32, i32* @.const.3
 store i32 %ll.main.1.1.6, i32* %ll.main.1.0
 br label %ll.main.1.1.7
ll.main.1.1.7:
 %ll.main.1.1.8 = load i32, i32* %ll.main.1.0
 %ll.main.1.2 = load i32, i32* @.const.3
 %ll.main.1.3 = icmp eq i32 %ll.main.1.1.8, %ll.main.1.2
 br i1 %ll.main.1.3, label %ll.main.1.4, label %ll.main.1.6
ll.main.1.4:
 %ll.main.1.5 = load i32, i32* @.const.4
 store i32 %ll.main.1.5, i32* %ll.main.0
 br label %ll.main.1.8
ll.main.1.6:
 %ll.main.1.7 = load i32, i32* @.const.5
 store i32 %ll.main.1.7, i32* %ll.main.0
 br label %ll.main.1.8
ll.main.1.8:
 %ll.main.1.9 = load i32, i32* %ll.main.0
 ret i32 %ll.main.1.9
}
