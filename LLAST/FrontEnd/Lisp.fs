module LL.Lisp

module IR = open LL.IR
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
let op o s = cnst o <@> str s
let flip f a b = f b a
let oneOf = List.reduce either
let rec deftype = 
    let middle = curry IR.DefTy <@> name <*> typeLit
    l *> str "defty" *> middle <* r

and int = (fun token -> Int32.Parse (token.value) ) <@> term
and typeLit xs = 
    let typeLitList = many typeLit
    let agg =
        IR.Agg <@> (listl *> typeLitList <* listr)
    let arr = (curry IR.Arr) <@> l *> str "Arr" *> int <*> typeLit <* r
    let vec = (curry IR.Vec) <@> l *> str "Vec" *> int <*> typeLit <* r
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
    let nametypetuple = l *> name <* (str ":") <.> typeLit <* r
    let middle = many nametypetuple
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
                    (IR.Eq,  "==")
                    (IR.Gt,  ">")
                    (IR.Ge,  ">=")
                    (IR.Lt,  "<")
                    (IR.Le,  "<=")
                    (IR.Ne,  "!=")
                ]

(* constants *)
and ud = (flip << curry) IR.UD <@> (UInt64.Parse <@> name) <* str "u" <*> (Int32.Parse <@> name)
and irid = (flip << curry) IR.ID <@> (Int64.Parse <@> name) <* str "i" <*> (Int32.Parse <@> name)
and fd = (flip << curry) IR.FD <@> (Double.Parse <@> name) <* str "f" <*> (Int32.Parse <@> name)
and arrd = IR.ArrD <@> l *> str "ArrD" *> many constants <* r
and vecd = IR.VecD <@> l *> str "VecD" *> many constants <* r
and aggd = IR.AggD <@> listl *> many constants <* listr
and undef = IR.Undef <@> l *> str "undef" *> typeLit <* r

and constants xs = List.reduce either [ud; irid; fd; arrd; vecd; aggd; undef] xs
and const' = IR.Const <@> constants
and llvm = oneOf
                <| List.append [
                    
                    deftype
                    suite
                    lambda
                    ifte
                    whil
                    const'
                ] binOps

let lex text = 
   
    let tokens = lex <| Some castMap <| lexerTB <| {filename = ""; text = text}
    Seq.filter
    <| fun (it: token) -> it.name <> "space"
    <| tokens
    |> fun it -> {arr = Array.ofSeq it; offset = 0}
    



    


