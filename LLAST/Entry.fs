module Entry

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
    let llvm = Parser.parse llvm (lex source)
    printfn "%A" llvm

[<EntryPoint>]
let main args =
   
    //parse code
    let sourcePath = args.[0]
    let llPath = sourcePath + ".ll"
    let source = System.IO.File.ReadAllText(sourcePath)
    try
        let llvm = Parser.parse llvm (lex source)
        let ctx = context.init
        let type_table = hashtable()
        let emit' = emit <| type_table
        try
        let _, proc = emit' ctx <| visit ctx (fun ctx -> elimIfElse ctx >> elimWhile ctx) llvm
        System.IO.File.WriteAllText(llPath, proc.to_ir)
        with LLException(exc) ->
        printfn "compilation failed"
        printfn "%A" exc
    with exc -> 
        printfn "Parse failed"
        printfn "%A" exc
    
    


    0
