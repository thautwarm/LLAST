module LL.Lisp

module IR = open LL.IR
open FastParse.Infras
open FastParse.Lexer
open FastParse.Parser
open LL
let lexerTB = 
    [
        R "term" "[^\(\)\s\[\]]+"
        C "paren" ["("; ")"; "["; "]"]
        R "space" "\s+"
    ]

let castMap = 
    Map 
    <|
    [
        "defun", "keyword"
        "defty", "keyword"
    ]
 
let between l parser r =
    let p = both l parser <| fun _ it -> it
    both p r <| fun it _ -> it 

let defun' = token_by_value_addr "define"
let defty' = token_by_value_addr "defty"

let term = token_by_name "term"

let l = token_by_value "("
let r = token_by_value ")"
let listl = token_by_value "["
let listr = token_by_value "]"

let id e = e

let rec deftype = 
    let middle = both 
                 <| (both defty' term <| fun _ it -> it.value)
                 <| ty_literal_lst 
                 <| fun name type_lst -> IR.DefTy(name,type_lst)
    between l middle r
and ty_literal_lst tokens = 
        rep ty_literal 1 -1 id tokens
and ty_literal = 
    

    let pa = 
        between listl ty_literal_lst listr
        |> trans 
        <| fun it -> IR.Agg it 
    let pb = trans term <| fun token ->
         match token.value with 
         | "i32" -> IR.I 32
         | it    -> IR.Alias it
    either pa pb
and suite = 
    let middle token = rep llvm 1 -1 (fun xs -> IR.Suite(xs)) token
    between listl middle listr
and llvm = List.reduce either
                <| [
                    deftype
                    suite
                ]

let lex text = 
   
    let tokens = lex <| Some castMap <| lexerTB <| {filename = ""; text = text}
    Seq.filter
    <| fun (it: token) -> it.name <> "space"
    <| tokens
    |> fun it -> {arr = Array.ofSeq it; offset = 0}
    



    


