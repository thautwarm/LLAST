module LL.Lisp

module IR = open LL.IR
open FastParse.Infras
open FastParse.Lexer
open FastParse.Parser
open LL
let lexerTB = 
    [
        R "term" "[^\(\)\s\[\]:]+"
        C "paren" ["("; ")"; "["; "]"; ":"]
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
let fst a _ = a
let snd _ b = b
let (<*) a b = both a b fst
let (*>) a b = both a b snd
let (<.>) a b = both a b (fun a b -> a,b)
let (<*>) a b = both a b (<|)
let (<@>) f a = trans a f
let (<|>) = either
let liftA2 a b c = both b c a
let str s = trans (token_by_value s) <| fun token -> token.value
let name = (fun a -> a.value) <@> term
let many p = rep p 1 (-1) id
let rec deftype = 
    let middle = both 
                 <| (both defty' term <| fun _ it -> it.value)
                 <| ty_literal_lst 
                 <| fun name type_lst -> IR.DefTy(name,type_lst)
    l *> middle <* r
and ty_literal_lst tokens = 
        rep ty_literal 1 -1 id tokens
and ty_literal = 
    let pa = 
        IR.Agg <@> <| listl *> ty_literal_lst <* listr
        
    let pb = term <@> <| fun token ->
         match token.value with 
         | "i32" -> IR.I 32
         | it    -> IR.Alias it
    pa <|> pb
and suite = 
    let middle token = rep llvm 1 -1 (fun xs -> IR.Suite(xs)) token
    between listl middle listr
and lambda xs = 
    let nametypetuple = l *> name <* (str ":") <.> ty_literal <* r
    let middle = many nametypetuple
    let args = listl *> middle <* listr
    let middle = IR.Lambda <@> args <*> ty_literal <*> llvm
    l *> str "lambda" *> middle <* r
        <| xs


and llvm = List.reduce either
                <| [
                    deftype
                    suite
                    lambda
                ]

let lex text = 
   
    let tokens = lex <| Some castMap <| lexerTB <| {filename = ""; text = text}
    Seq.filter
    <| fun (it: token) -> it.name <> "space"
    <| tokens
    |> fun it -> {arr = Array.ofSeq it; offset = 0}
    



    


