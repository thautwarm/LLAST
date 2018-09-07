@.const.0 = private unnamed_addr constant i32 0, align 4
@.const.1 = private unnamed_addr constant i32 123, align 4
@.const.2 = private unnamed_addr constant i32 1, align 4
@.const.3 = private unnamed_addr constant i32 2, align 4
define i32 @main(){
 %ll.main.0 = load i32, i32* @.const.0
 %ll.main.1 = alloca i32, align 4
 store i32 %ll.main.0, i32* %ll.main.1
 %ll.main.2.0 = load i32, i32* @.const.0
 %ll.main.2.1 = alloca i32, align 4
 store i32 %ll.main.2.0, i32* %ll.main.2.1
 br label %ll.main.2.2.0
ll.main.2.2.0:
 %ll.main.2.2.1 = load i32, i32* %ll.main.2.1
 %ll.main.2.2.2 = load i32, i32* @.const.1
 %ll.main.2.2.3 = icmp slt i32 %ll.main.2.2.1, %ll.main.2.2.2
 br i1 %ll.main.2.2.3, label %ll.main.2.2.4, label %ll.main.2.2.11
ll.main.2.2.4:
 %ll.main.2.2.5 = load i32, i32* %ll.main.2.1
 %ll.main.2.2.6 = load i32, i32* @.const.2
 %ll.main.2.2.7 = add i32 %ll.main.2.2.5, %ll.main.2.2.6
 store i32 %ll.main.2.2.7, i32* %ll.main.2.1
 %ll.main.2.2.8 = load i32, i32* %ll.main.1
 %ll.main.2.2.9 = load i32, i32* @.const.3
 %ll.main.2.2.10 = add i32 %ll.main.2.2.8, %ll.main.2.2.9
 store i32 %ll.main.2.2.10, i32* %ll.main.1
 br label %ll.main.2.2.0
ll.main.2.2.11:
 %ll.main.2.2.12 = load i32, i32* %ll.main.2.1
 ret i32 %ll.main.2.2.12
}
