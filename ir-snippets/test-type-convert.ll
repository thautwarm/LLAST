define float @func(i32 %.func.arg1){
 %.func.0 = add i32 %.func.arg1, %.func.arg1
 %.func.1 = sitofp i32 %.func.0 to float
 ret float %.func.1
}
