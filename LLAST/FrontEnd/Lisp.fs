module LL.Lisp

module IR =
    open LL.IR

open FastParse.Infras
open FastParse.Lexer
open FastParse.Parser
open LL
open System


let erro_report (token: token) msg =
    failwithf "Error occurs at <%s>: %s at line %d, column %d, file %s." token.value msg token.lineno token.colno token.filename


let parse_str (str: string) = 
    let rec parse_str =
        function
        | [] -> []
        | [it] -> [it]
        | ('\\' ::  x :: xs | x :: xs)-> x :: parse_str xs
    let n = str.Length
    [for i = 1 to n - 2 do yield str.[i]] |> parse_str
    |> Array.ofList
    |> System.String

(** parse literal data *)
let (|FD|ID|UD|NotData|) (tk: token) =
    match tk.value with
    | "true" -> ID(1, 1L)
    | "false"-> ID(1, 0L)
    | str ->
    let idx = str.IndexOfAny([|'u'; 'f'; 'i'|])
    match idx with
    | -1 ->
       try ID(32, Int64.Parse str) with :? System.FormatException ->
       try FD(64, Double.Parse str) with :? System.FormatException ->
       NotData
    | it ->
    try
        let value, sign, bit = str.[..it-1], str.[it], str.[it + 1..]
        match sign with
        | 'u' -> UD(Int32.Parse bit, UInt64.Parse value)
        | 'i' -> ID(Int32.Parse bit, Int64.Parse value)
        | 'f' -> FD(Int32.Parse bit, Double.Parse value)
        | _ -> NotData
    with _ -> NotData

let lexerTB =

    [
      R "cstr"  "\"([^\\\"]+|\\\.)*?\""
      R "term" "[^\(\)\s\[\]\"]+"
      C "paren" [ "("; ")"; "["; "]";]
      R "space" "\s+" ]


let mutable castMap =
    Map <| []

let keyword s =
    castMap <- Map.add s "keyword" castMap
    token_by_value_addr s

(** keyword token *)

let def' = keyword "def"
let lamdba' = keyword "lambda"
let decl'  = keyword "decl"
let while' = keyword "while"
let indrbr' = keyword "indrbr"
let arr' = keyword "arr"
let vec' = keyword "vec"
let ptr' = keyword "ptr"
let void' = keyword "void"
let switch' = keyword "switch"
let ret' = keyword "ret"
let label' = keyword "label"
let br' = keyword "br"
let jump' = keyword "jump"
let zext' = keyword "zext"
let convert' = keyword "convert"
let bitcast' = keyword "bitcast"
let if' = keyword "if"
let let' = keyword "let"
let lsh' = keyword "lsh"
let lshr' = keyword "lshr"
let ashr' = keyword "ashr"
let and' = keyword "and"
let or' = keyword "or"
let xor' = keyword "xor"
let type' = keyword "type"
let alloca' = keyword "alloca"
let store' = keyword "store"
let load' = keyword "load"
let gep' = keyword "gep"
let undef' = keyword "undef"
let agg' = keyword "agg"
let ifte' = keyword "if"
let add'  = keyword "+"
let mul' = keyword "*"
let sub' = keyword "-"
let rem' = keyword "%"
let eq' = keyword "=="
let neq' = keyword "!="
let gt' = keyword ">"
let lt' = keyword "<"
let ge' = keyword ">="
let le' = keyword "<="
let suite' = keyword "suite"
let cstr' = keyword "str"

(** end keyword token *)

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
let ( *> ) a b = both a b snd
let (<.>) a b = both a b <| fun a b -> a, b
let (<*>) a b = both a b (<|)
let (<@>) f a = trans a f
let (<|>) = either
let liftA2 a b c = both b c a
let str s = trans (token_by_value s) <| fun token -> token.value
let name = (fun a -> a.value) <@> (pred (fun (it: token) -> it.name = "term") term)
let many  p = rep p 1 (-1) id
let many0 p = rep p 0 (-1) id
let curry f a b = f (a, b)
let curr3 f a b c = f (a, b, c)
let curr4 f a b c d = f (a, b, c, d)
let br a = l *> a <* r
let cnst a _ = a
let pure' a = cnst a <@> str ""
let op o s = cnst o <@> str s
let flip f a b = f b a
let cstr =  (fun a -> IR.CStr(parse_str a.value)) <@> token_by_name "cstr"
let oneOf xs = List.reduce either xs

let rec llvm =
    oneOf
    <|
     [
        (** s-expr startswith keyword *)
        l *> pgen keyword_dispatch <* r

        (** a sequence of s-expr*)
        suite

        (** literal constant *)
        IR.Const <@> constant

        (** load symbol *)
        get

        (** application *)
        app
     ]

and app = 
    locate <| fun xs -> curry IR.App <@> l *> llvm <*> many0 llvm <* r <| xs

and get = 
    IR.Get <@> name
    |> locate

and suite =
    let middle token = rep llvm 1 -1 IR.Suite token
    l *> suite' *> middle <* r
    |> locate


and locate (parser: IR.llvm parser) =
            fun tokens ->
            let offset = tokens.offset
            parser tokens >>=
            fun (a, tokens) ->
            let token = tokens.arr.[offset]
            let located = 
                IR.Locate({lineno = token.lineno; colno = token.colno; filename = token.filename}, a)
            Just(located, tokens)
            

(** use `keyword` to dispatch s-expr with specific semantics *)
and keyword_dispatch =
    let sign =
       token_by_name "keyword"
    let dispatch tokens =
        sign tokens >>=
        function
        | (token, tokens) ->
        let return' (parser: IR.llvm parser) =
            let new_parser tokens =
                parser tokens >>=
                fun (a, tokens) ->
                Just(IR.Locate({lineno = token.lineno; colno = token.colno; filename = token.filename}, a), tokens)
            Just(new_parser, tokens)

        match token.value with
        | "lambda"  -> return'  lambda
        | "decl"    -> return'  decl
        | "while"   -> return'  whil
        | "let"     -> return'  let_exp
        | "indrbr"  -> return'  indrbr
        | "switch"  -> return' switch
        | "ret"     -> return' ret
        | "label"   -> return' label
        | "br"      -> return' branch
        | "jump"    -> return' jump
        | "zext"    -> return' zext

        | "convert" -> return' convert
        | "bitcast" -> return' bitcast
        | "if"      -> return' ifte
        | "alloca"  -> return' alloca
        | "load"    -> return' load
        | "store"   -> return' store
        | "gep"     -> return' gep
        (** constant *)
        | "arr"     -> IR.Const <@> arrD |> return'
        | "vec"     -> IR.Const <@> vecD |> return'
        | "agg"     -> IR.Const <@> aggD |> return'
        (** bin *)
        | "lsh"     -> return' <| bin IR.LSh
        | "lshr"    -> return' <| bin IR.LShr
        | "ashr"    -> return' <| bin IR.AShr
        | "and"     -> return' <| bin IR.And
        | "or"      -> return' <| bin IR.Or
        | "xor"     -> return' <| bin IR.XOr
        | "+"       -> return' <| bin IR.Add
        | "-"       -> return' <| bin IR.Sub
        | "*"       -> return' <| bin IR.Mul
        | "%"       -> return' <| bin IR.Rem
        | "=="      -> return' <| bin IR.Eq
        | "!="      -> return' <| bin IR.Ne
        | ">"       -> return' <| bin IR.Gt
        | "<"       -> return' <| bin IR.Lt
        | ">="      -> return' <| bin IR.Ge
        | "<="      -> return' <| bin IR.Le
        | "def"     -> return' def
        | _         -> Nothing
    dispatch

and def =
    let deftype =
        curry IR.DefTy <@> name <*>  l *> type' *> typeLit <* r

    let defun xs =
        let arg = l *> name <.>  typeLit <* r
        let args = listl *> many0 arg <* listr
        curr4 IR.Defun <@> name <*> args <*> typeLit <*> llvm
        <| xs

    let defvar =
        curry IR.DefVar <@> name <*> constant

    either deftype
    <| either defun defvar

and let_exp = curr3 IR.Let <@> name <*> llvm <*> llvm

and decl xs =
    curr3 IR.Decl <@> name <*> listl *> many typeLit <* listr <*> typeLit
    <| xs

and switch xs =
    let cases = l *> llvm <.> name <* r
    curr3 IR.Switch <@>
    llvm <*> listl *> many cases <* listr <*> name
    <| xs

and ret xs = IR.Return <@> llvm <| xs

and label = IR.Mark <@> name

and branch xs = curr3 IR.Branch <@> llvm <*> name <*> name <| xs

and jump = IR.Jump <@> name

and bin op =
    fun a b -> IR.Bin(op, a, b)
    <@> llvm <*> llvm

and zext xs = curry IR.ZeroExt <@> llvm <*> typeLit <| xs

and convert xs = curry IR.CompatCast <@> llvm <*> typeLit <| xs

and bitcast xs = curry IR.Bitcast <@> llvm <*> typeLit <| xs

and ifte xs =
    curr3 IR.IfExp <@>
    llvm <*> llvm <*> llvm
    <| xs


and indrbr xs =
    curry IR.IndrBr <@> llvm <*> listl *> many name <* listr
    <| xs

and whil xs = curry IR.WhileExp <@> llvm <*> llvm <| xs

and lambda xs =
    let middle = many0 (l *> name <.> typeLit <* r)
    let args = listl *> middle <* listr
    curr3 IR.Lambda <@> args <*> typeLit <*> llvm
    <| xs

and alloca = IR.Alloca <@> typeLit

and load = IR.Load <@> llvm

and store = curry IR.Store <@> llvm <*> llvm

and gep = curr3 IR.GEP <@> llvm <*> llvm <*> many int32'

//////////////////////////////////////////////////////////

and arrD = IR.ArrD <@> l *> arr' *> many constant <* r

and vecD = IR.VecD <@> l *> vec' *> many constant <* r

and aggD = IR.AggD <@> l *> agg' *> many constant <* r

and int32' = Int32.Parse <@> name

and num tokens =
    anyToken tokens >>=
    fun (token, arr) ->
        let return' it = Just(it, arr)
        match token with
        | ID(bit, value) -> return' <| IR.ID(bit, value)
        | FD(bit, value) -> return' <| IR.FD(bit, value)
        | UD(bit, value) -> return' <| IR.UD(bit, value)
        | NotData -> Nothing

and undef = IR.Undef <@> l *> undef' *> typeLit <* r

and constant xs = oneOf [cstr; num; arrD; vecD; aggD; undef ] <| xs

and typeLit xs =
    let typeLitList = many typeLit
    let agg = IR.Agg         <@> l *> agg' *> typeLitList <* r
    let arr = (curry IR.Arr) <@> l *> arr' *> int32' <*> typeLit <* r
    let vec = (curry IR.Vec) <@> l *> vec' *> int32' <*> typeLit <* r
    let func = (curry IR.Func) <@> listl *> (str "->" *> typeLitList) <*> typeLit <* listr
    let ptr = IR.Ptr <@> l *> ptr' *> typeLit <* r
    let void' = cnst IR.Void <@> void'
    let atom =
        (fun token ->
        match token.value with
        | "bool" -> IR.I 1
        | "i32" -> IR.I 32
        | "i8" -> IR.I 8
        | "i64" -> IR.I 64
        | "i16" -> IR.I 16
        | "u32" -> IR.U 32
        | "u8" -> IR.U 8
        | "u64" -> IR.U 64
        | "u16" -> IR.U 16
        | "f32" -> IR.F 32
        | "f64" -> IR.F 64
        | it -> IR.Alias it)
        <@> term
    List.reduce either [ agg; atom; arr; vec; func; ptr; void' ] <| xs



let lex text =
    let tokens = lex <| Some castMap <| lexerTB <| {filename = ""; text = text}
    Seq.filter
    <| fun (it: token) -> it.name <> "space"
    <| tokens
    |> fun it -> {arr = Array.ofSeq it; offset = 0}
