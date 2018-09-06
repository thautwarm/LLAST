define float @main(i32 %ll.main.arg1){
 %ll.main.0 = add i32 %ll.main.arg1, %ll.main.arg1
 %ll.main.1 = sitofp i32 %ll.main.0 to float
 ret float %ll.main.1
}
