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


    printf "======================================\n"

    let formal_args = [("arg1", I 32)]
    let ret_ty = F 32
    let body = 
        Let("var", 
            Conv(CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32)), 
            Bin(Add, Get("var"), Const(FD(32, 10.0))))
    let def_test1 =  Defun("test1", formal_args, ret_ty, body)


    let formal_args = [("arg1", I 32)]
    let ret_ty = I 64
    let body = 
        Conv(
            CompatCast(
                App(Get("test1"), 
                    [Get("arg1")])
                    , I 64))
    
    let def_test2 = Defun("test2", formal_args, ret_ty, body)
    
    let ctx = context.init
    let proc = ref Empty
    let emit' = emit <| hashtable() <| proc
    emit' ctx <| Suite([def_test2; def_test1]) |> ignore


    printf "%s" proc.Value.to_ir


    0


