@.const.0 = private unnamed_addr constant i8* blockaddress(@main, %ll.main.1.5), align 8
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)
@.const.1 = private unnamed_addr constant i32 10, align 4
define i32 @main(i32 %ll.main.arg1){
 %ll.main.0 = alloca i32, align 4
 %ll.main.1.0 = alloca i8*, align 8
 %ll.main.1.1 = bitcast i8** %ll.main.1.0 to i8*
 call void @llvm.memcpy.p0i8.p0i8.i64(i8* %ll.main.1.1, i8* bitcast(i8** @.const.0 to i8*), i64 8, i32 8, i1 false)
 %ll.main.1.2 = load i8*, i8** %ll.main.1.0
 %ll.main.1.3 = load i32, i32* @.const.1
 %ll.main.1.4 = icmp eq i32 %ll.main.1.3, %ll.main.arg1
 br i1 %ll.main.1.4, label %ll.main.1.5, label %ll.main.1.6
ll.main.1.5:
 br label %ll.main.1.8
ll.main.1.6:
 %ll.main.1.7 = load i32, i32* @.const.1
 store i32 %ll.main.1.7, i32* %ll.main.0
 br label %ll.main.1.8
ll.main.1.8:
 %ll.main.1.9 = load i32, i32* %ll.main.0
 ret i32 %ll.main.1.9
}
