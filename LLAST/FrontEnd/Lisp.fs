﻿module LL.Lisp

module IR = open LL.IR
open System
open FastParse.Infras
open FastParse.Lexer
open FastParse.Parser
open LL
open System
open System.Net.Sockets
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
let term = token_by_name "term"

let l = token_by_value "("
let r = token_by_value ")"
let listl = token_by_value "["
let listr = token_by_value "]"
let id e = e
let fst a _ = a
let snd _ b = b
let (<*) a b = both a b fst
let ( *>) a b = both a b snd
let (<.>) a b = both a b <| fun a b -> a,b
let (<*>) a b = both a b (<|)
let (<@>) f a = trans a f
let (<|>) = either
let liftA2 a b c = both b c a
let str s = trans (token_by_value s) <| fun token -> token.value
let name = (fun a -> a.value) <@> term
let many p = rep p 1 (-1) id
let curry f a b = f(a,b)
let curr3 f a b c = f(a,b,c)
let curr4 f a b c d = f(a,b,c,d)
let br a = l *> a <* r
let cnst a _ = a
let pure' a = cnst a <@> str ""
let op o s = cnst o <@> str s
let flip f a b = f b a
let oneOf xs = List.reduce either xs
let rec deftype = 
    let middle = curry IR.DefTy <@> name <*> typeLit
    l *> str "defty" *> middle <* r

and typeLit xs = 
    let typeLitList = many typeLit
    let agg =
        IR.Agg <@> (listl *> typeLitList <* listr)
    let arr = (curry IR.Arr) <@> l *> str "Arr" *> int32' <*> typeLit <* r
    let vec = (curry IR.Vec) <@> l *> str "Vec" *> int32' <*> typeLit <* r
    let func = (curry IR.Func) <@> l *> typeLitList <* str "->" <*> typeLit <* r
    let ptr = IR.Ptr <@> l *> str "ptr" *> typeLit <* r
    let voi = cnst IR.Void <@> str "void"

    let atom = (fun token ->
                match token.value with 
                | "bool"-> IR.I 1
                | "i32" -> IR.I 32
                | "i8"  -> IR.I 8
                | "i64" -> IR.I 64
                | "i16" -> IR.I 16
                | "u32" -> IR.U 32
                | "u8"  -> IR.U 8
                | "u64" -> IR.U 64
                | "u16" -> IR.U 16
                | "f32" -> IR.F 32
                | "f64" -> IR.F 64
                
                | it    -> IR.Alias it) <@> term
    List.reduce either [agg; atom; arr; vec; func; ptr; voi] <| xs
        
and suite = 
    let middle token = rep llvm 1 -1 IR.Suite token
    listl *> middle <* listr


and lambda xs = 
    let middle = many ( l *> name <* (str ":") <.> typeLit <* r)
    let args = listl *> middle <* listr
    let lambda' a b c = IR.Lambda(a,b,c)
    let middle = lambda' <@> args <*> typeLit <*> llvm
    l *> str "lambda" *> middle <* r
    <| xs        
and ifte xs =
    let ifte' a b c = IR.IfExp(a,b,c)
    ifte' <@> l *> str "if" *> llvm <*> llvm <*> llvm <* r
        <| xs
and whil xs = 
    curry IR.WhileExp <@> l *> str "while" *> llvm <*> llvm <* r 
    <| xs
and bin (o, s) = fun xs -> curr3 IR.Bin <@> l *> op o s <*> llvm <*> llvm  <* r <| xs
and binOps = List.map bin 
                [
                    (IR.Add, "+")
                    (IR.Sub, "-")
                    (IR.Mul, "*")
                    (IR.Div, "/")
                    (IR.Rem, "%")
                    (IR.LSh, "lsh")
                    (IR.LShr,"lshr")
                    (IR.LShr,"ashr")
                    (IR.And, "and")
                    (IR.Or,  "or")
                    (IR.XOr, "xor")
                    (IR.Eq,  "==")
                    (IR.Gt,  ">")
                    (IR.Ge,  ">=")
                    (IR.Lt,  "<")
                    (IR.Le,  "<=")
                    (IR.Ne,  "!=")
                ]

(* constants *)
and int32' = try Int32.Parse <@> name with _ -> cnst Nothing
and double' = try Double.Parse <@> name with _ -> cnst Nothing
and int64' = try Int64.Parse <@> name with _ -> cnst Nothing
and uint64' = try UInt64.Parse <@> name with _ -> cnst Nothing
and fd = (fun a b -> IR.FD(b,a)) <@> double' <* str "f" <*> int32'
and ud = (fun a b -> IR.UD(b,a)) <@> uint64' <* str "u" <*> int32'
and irid = (fun a b -> IR.ID(b,a)) <@> int64' <* str "i" <*> int32'
and arrd = IR.ArrD <@> l *> str "ArrD" *> many constants <* r
and vecd = IR.VecD <@> l *> str "VecD" *> many constants <* r
and aggd = IR.AggD <@> listl *> many constants <* listr
and undef = IR.Undef <@> l *> str "undef" *> typeLit <* r

and constants xs = oneOf [ud; irid; fd; arrd; vecd; aggd; undef] xs
and const' = IR.Const <@> constants
and app xs = curry IR.App <@> l *> llvm <*> many llvm <* r <| xs
and get xs = IR.Get <@> name <| xs
and alloca = IR.Alloca <@> l *> str "alloca" *> typeLit <* r
and load = IR.Load <@> l *> str "load" *> llvm <* r
and store = curry IR.Store <@> l *> str "store" *> llvm <*> llvm <* r
and gep = curr3 IR.GEP <@> l *> str "gep" *> llvm <*> llvm <*> many int32' <* r
and llvm = oneOf
                <| List.append binOps [
                    deftype
                    suite
                    lambda
                    ifte
                    whil
                    get
                    const'
                    app
                ]

let lex text = 
   
    let tokens = lex <| Some castMap <| lexerTB <| {filename = ""; text = text}
    Seq.filter
    <| fun (it: token) -> it.name <> "space"
    <| tokens
    |> fun it -> {arr = Array.ofSeq it; offset = 0}
    



    


