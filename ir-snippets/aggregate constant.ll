 @.const.0 = private unnamed_addr constant { i32, i8 } { i32 10, i8 10 }, align 4
 declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)
%.struct.redyred = type { i32, i8 }
define i32 @main(){
 %ll.main.0 = alloca { i32, i8 }, align 4
 %ll.main.1 = bitcast { i32, i8 }* %ll.main.0 to i8*
 call void @llvm.memcpy.p0i8.p0i8.i64(i8* %ll.main.1, i8* bitcast({ i32, i8 }* @.const.0 to i8*), i64 8, i32 4, i1 false)
 %ll.main.2 = load { i32, i8 }, { i32, i8 }* %ll.main.0
 %ll.main.3.0 = extractvalue { i32, i8 } %ll.main.2, 0
 ret i32 %ll.main.3.0
}
