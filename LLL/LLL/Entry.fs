open LLL.LLVM.ML
open LLL.LLVM.MLEmit
open LLL.LLVM.Helper

[<EntryPoint>]
let main args = 
    let formal_args = [("arg1", I 32)]
    let ret_ty = I 32
    let body = Bin(Add, Get "arg1", Get "arg1")
    let ctx = context.init
    let proc = ref Empty
    let emit' = emit <| hashtable() <| proc
    emit' ctx <| Defun("func", formal_args, ret_ty, body) |> ignore
    printf "%s" proc.Value.to_ir


    let formal_args = [("arg1", I 32)]
    let ret_ty = F 32
    let body = Conv(CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32))
    let ctx = context.init
    let proc = ref Empty
    let emit' = emit <| hashtable() <| proc
    emit' ctx <| Defun("func", formal_args, ret_ty, body) |> ignore
    printf "%s" proc.Value.to_ir
    0


