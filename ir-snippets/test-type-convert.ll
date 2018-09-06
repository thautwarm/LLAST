define float @func(i32 %ll.func.arg1){
 %ll.func.0 = add i32 %ll.func.arg1, %ll.func.arg1
 %ll.func.1 = sitofp i32 %ll.func.0 to float
 ret float %ll.func.1
}
