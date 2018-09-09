module Tests

open System
open Xunit
open LL.IR
open LL.Emit
open LL.Infras
open LL.Helper
open LL.Exc
open LL.Emit
open LL.Exc
open LL.Helper
open LL.IR
open LL.Infras
open LL.Pass
open LL.ControlFlow
open System.IO

open LL.Lisp
open FastParse


let codegen (title : string) (llvm : llvm) : unit =
    let ctx = context.init
    let type_table = hashtable()
    let emit' = emit <| type_table
    try
    let _, proc = emit' ctx <| visit ctx (fun ctx -> elimIfElse ctx >> elimWhile ctx) llvm
    System.IO.File.WriteAllText(fmt "../ir-snippets/%s.ll" title, proc.to_ir)
    with LLException(exc) ->
        printfn "test %s failed" title
        printfn "%A" exc


let parse source =
    try
        let llvm = Parser.parse llvm (lex source)
        
        let ctx = context.init
        let type_table = hashtable()
        let emit' = emit <| type_table
        try
        let _, proc = emit' ctx <| visit ctx (fun ctx -> elimIfElse ctx >> elimWhile ctx) llvm
        printfn "%s" <| proc.to_ir
        with LLException(exc) ->
        printfn "compilation failed"
        printfn "%A" exc
    with exc -> 
        printfn "Parse failed"
        printfn "%A" exc
    

let test (title: string) (ir: llvm) =
    printf "============%s===============\n" title
    let ctx = context.init
    let type_table = hashtable()
    let emit' = emit <| type_table
    try
        let _, proc = emit' ctx <| ir
        printf "%s" proc.to_ir
    with LLException(exc) ->
        printf "%A" exc

[<Fact>]
let ``My test`` () =
    let formal_args = [("arg1", I 32)]
    let ret_ty = I 32
    let body = Bin(Add, Get "arg1", Get "arg1")
    let defun = Defun("func", formal_args, ret_ty, body)
    test "simple-defun" defun


    let formal_args = [("arg1", I 32)]
    let ret_ty = F 32
    let body = CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32)

    let defun = Defun("func", formal_args, ret_ty, body)
    test "test-type-convert" defun

    let formal_args = [("arg1", I 32)]
    let ret_ty = F 32
    let body =
        Let("var",
            CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32),
            Bin(Add, Get("var"), Const(FD(32, 10.0))))
    let def_test1 =  Defun("test1", formal_args, ret_ty, body)


    let formal_args = [("arg1", I 32)]
    let ret_ty = I 64
    let body =
        CompatCast(
            App(Get("test1"),
                [Get("arg1")])
                , I 64)

    let def_test2 = Defun("test2", formal_args, ret_ty, body)

    let suite = Suite([def_test2; def_test1])
    test "cross-definition" suite

    let formal_args = [("arg1", I 32)]
    let ret_ty = I 32

    let cond = Bin(Eq, Const <| ID(32, 10L), Get("arg1"))
    let branch = Branch(cond, "truelabel", "falselabel")

    let iftrue = Suite([Mark "truelabel"; Jump "tag"])

    let iffalse = Suite([Mark "falselabel"; Store(Get "result", Const <| ID(32, 10L))])

    let ifresult = Let("result", Alloca(I 32), Suite([branch; iftrue; iffalse; Load(Get "result")]))

    let ret = Suite([Return ifresult; Mark "tag"; Const(ID(32, 15L)) |> Return])

    let whole = Defun("test3", formal_args, ret_ty,
                      ret)
    test "jump" whole


    //let ty_def = DefTy("master", Agg [I 1; I 8; F 32; Agg([I 32; I 64])])
    //let defun  =
    //    Defun(
    //        "main",
    //        [],
    //        I 64,
    //        ExtractVal(
    //              Let("value",
    //                  Alloca(Alias "master"),
    //                  Load <| GEP(Get "value", Const <| ID(32, 0L), [])
    //                  ),
    //              [3; 1])
    //        )
    //test "member-accessing" <| Suite [ty_def; defun]

    

    
    0
