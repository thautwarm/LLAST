open LLL.LLVM.Emit
open LLL.LLVM.IR
[<EntryPoint>]
let main args = 
    let formal_args = [("arg1", I 32)]
    let ret_ty = I 32
    let body = Bin(Add, Get "arg1", Get "arg1")
    let context = {``global`` = hashtable(); local = hashtable(); count = 0; identifier = []}
    let compiler = {codes = arraylist();}
    let inner = emit compiler
    inner context <| Define ("func", formal_args, ret_ty, body) |> ignore
    System.Console.WriteLine(compiler.ToString())

    let formal_args = [("arg1", F 32)]
    let ret_ty = F 32
    let body = Bin(Add, Get "arg1", Get "arg1")
    let context = {``global`` = hashtable(); local = hashtable(); count = 0; identifier = []}
    let compiler = {codes = arraylist();}
    let inner = emit compiler
    inner context <| Define ("func", formal_args, ret_ty, body) |> ignore
    System.Console.WriteLine(compiler.ToString())



    let formal_args = [("arg1", F 64)]
    let ret_ty = F 32
    let body = Conv(FloatTrunc(Bin(Add, Get "arg1", Get "arg1"), F 32))
    let context = {``global`` = hashtable(); local = hashtable(); count = 0; identifier = []}
    let compiler = {codes = arraylist();}
    let inner = emit compiler
    inner context <| Define ("func", formal_args, ret_ty, body) |> ignore
    System.Console.WriteLine(compiler.ToString())


    0


