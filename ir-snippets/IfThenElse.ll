@.const.0 = private unnamed_addr constant i1 1, align 1
@.const.1 = private unnamed_addr constant i1 0, align 1
@.const.2 = private unnamed_addr constant i32 123, align 4
@.const.3 = private unnamed_addr constant i32 456, align 4
@.const.4 = private unnamed_addr constant i32 111, align 4
@.const.5 = private unnamed_addr constant i32 222, align 4
define i32 @main(i32 %ll.main.arg1){
 %ll.main.0 = alloca i32, align 4
 %ll.main.1 = alloca i32, align 4
 %ll.main.2 = load i1, i1* @.const.0
 %ll.main.3 = load i1, i1* @.const.1
 %ll.main.4 = icmp eq i1 %ll.main.2, %ll.main.3
 br i1 %ll.main.4, label %ll.main.9.0, label %ll.main.9.1
ll.main.9.0:
 %ll.main.5 = load i32, i32* @.const.2
 store i32 %ll.main.5, i32* %ll.main.1
 br label %ll.main.9.2
ll.main.9.1:
 %ll.main.7 = load i32, i32* @.const.3
 store i32 %ll.main.7, i32* %ll.main.1
 br label %ll.main.9.2
ll.main.9.2:
 %ll.main.9.3 = load i32, i32* %ll.main.1
 %ll.main.10 = load i32, i32* @.const.3
 %ll.main.11 = icmp eq i32 %ll.main.9.3, %ll.main.10
 br i1 %ll.main.11, label %ll.main.16.0, label %ll.main.16.1
ll.main.16.0:
 %ll.main.12 = load i32, i32* @.const.4
 store i32 %ll.main.12, i32* %ll.main.0
 br label %ll.main.16.2
ll.main.16.1:
 %ll.main.14 = load i32, i32* @.const.5
 store i32 %ll.main.14, i32* %ll.main.0
 br label %ll.main.16.2
ll.main.16.2:
 %ll.main.16.3 = load i32, i32* %ll.main.0
 ret i32 %ll.main.16.3
}
