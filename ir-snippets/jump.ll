 @.const.0 = private unnamed_addr constant i8 * blockaddress(@test3, %truelabel), align 8
 declare void @llvm.memcpy.p0i8.p0i8.i64(i8 *, i8 *, i64, i32, i1)
 @.const.1 = private unnamed_addr constant i32 10, align 4
 @.const.2 = private unnamed_addr constant i32 15, align 4
define i32 @ifelsetest(i32 %.ifelsetest.arg1){
 %.ifelsetest.0 = alloca i32, align 4
 %.ifelsetest.1.0 = alloca i8 *, align 8
 %.ifelsetest.1.1 = bitcast i8 * * %.ifelsetest.1.0 to i8 *
 call void @llvm.memcpy.p0i8.p0i8.i64(i8 * %.ifelsetest.1.1, i8* bitcast(i8 * * @.const.0 to i8*), i64 8, i32 8, i1 false)
 %.ifelsetest.1.2 = load i8 *, i8 ** %.ifelsetest.1.0
 %.ifelsetest.1.3 = load i32, i32* @.const.1
 %.ifelsetest.1.4 = icmp eq i32 %.ifelsetest.1.3, %.ifelsetest.arg1
 br i1 %.ifelsetest.1.4, label %falselabel, label %truelabel
truelabel:
 br label %tag
falselabel:
 %.ifelsetest.1.5 = load i32, i32* @.const.1
 store i32 %.ifelsetest.1.5, i32 * %.ifelsetest.0
 %.ifelsetest.1.6 = load i32, i32* %.ifelsetest.0
 ret i32 %.ifelsetest.1.6
tag:
 %.ifelsetest.2 = load i32, i32* @.const.2
 ret i32 %.ifelsetest.2
}
