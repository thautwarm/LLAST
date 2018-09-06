 @.const.0 = private unnamed_addr constant i8* blockaddress(@test3, %ll.test3.1.5), align 8
 declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)
 @.const.1 = private unnamed_addr constant i32 10, align 4
define i32 @test3(i32 %ll.test3.arg1){
 %ll.test3.0 = alloca i32, align 4
 %ll.test3.1.0 = alloca i8*, align 8
 %ll.test3.1.1 = bitcast i8** %ll.test3.1.0 to i8*
 call void @llvm.memcpy.p0i8.p0i8.i64(i8* %ll.test3.1.1, i8* bitcast(i8** @.const.0 to i8*), i64 8, i32 8, i1 false)
 %ll.test3.1.2 = load i8*, i8** %ll.test3.1.0
 %ll.test3.1.3 = load i32, i32* @.const.1
 %ll.test3.1.4 = icmp eq i32 %ll.test3.1.3, %ll.test3.arg1
 br i1 %ll.test3.1.4, label %ll.test3.1.6, label %ll.test3.1.5
ll.test3.1.5:
 br label %ll.test3.1.8
ll.test3.1.6:
 %ll.test3.1.7 = load i32, i32* @.const.1
 store i32 %ll.test3.1.7, i32* %ll.test3.0
 br label %ll.test3.1.8
ll.test3.1.8:
 %ll.test3.1.9 = load i32, i32* %ll.test3.0
 ret i32 %ll.test3.1.9
}
