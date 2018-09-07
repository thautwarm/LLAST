@.const.0 = private unnamed_addr constant i32 0, align 4
%.struct.master = type { i1, i8, float, { i32, i64 } }
define i64 @main(){
 %ll.main.0 = alloca %.struct.master, align 8
 %ll.main.1.0 = load i32, i32* @.const.0
 %ll.main.1.1 = getelementptr inbounds %.struct.master, %.struct.master* %ll.main.0, i32 %ll.main.1.0
 %ll.main.1.2 = load %.struct.master, %.struct.master* %ll.main.1.1
 %ll.main.2 = extractvalue %.struct.master %ll.main.1.2, 3, 1
 ret i64 %ll.main.2
}
