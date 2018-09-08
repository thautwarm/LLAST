module LL.Lisp

module IR = open LL.IR
open FastParse.Infras
open FastParse.Lexer
open FastParse.Parser
let lexerTB = 
    [
        R "term" "[^\(\)\s]"
        C "paren" ["("; ")"]
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

let id e = e

let rec ty = 
    let pa = 
        between l ty_lst r
        |> trans 
        <| fun it -> IR.Agg it 
    let pb = trans term <| fun token ->
         match token.value with 
         | "i32" -> IR.I 32
         | it    -> IR.Alias it
    either pa pb
and ty_lst = 
    rep 
    <| ty
    <| 1 
    <| -1
    <| id



    


