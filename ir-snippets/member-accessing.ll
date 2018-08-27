 @.const.0 = private unnamed_addr constant i32 0, align 4
%.struct.master = type { i1, i8, float, { i32, i64 } }
define i64 @main(){
 %.main.0 = alloca %.struct.master, align 8
 %.main.value$1.0 = load i32, i32* @.const.0
 %.main.value$1.1 = getelementptr inbounds %.struct.master, %.struct.master * %.main.0, i32 %.main.value$1.0
 %.main.value$1.2 = load %.struct.master, %.struct.master* %.main.value$1.1
 %.main.2 = extractvalue %.struct.master %.main.value$1.2, 3, 1
 ret i64 %.main.2
}
