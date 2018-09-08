module LL.Pass
open LL.IR
open LL.Infras

let rec visit (ctx : context) (update : context -> llvm -> llvm) ast : llvm =
    update ctx <| fmap ctx (fun c -> visit c update) ast

and fmap ctx f ast =
    let pctx = f ctx
    match ast with
    | IfExp(cond, thenBlock, elseBlock)     -> IfExp(pctx cond, pctx thenBlock, pctx elseBlock)
    | WhileExp(cond, body)                  -> WhileExp(pctx cond, pctx body)
    | Bin(op, a, b)                         -> Bin(op, (f ctx) a, (f ctx) b)
    | App(a, xs)                            -> App((f ctx) a, List.map (f ctx) xs)
    | Let(s, a, b)                          -> Let(s, (f ctx) a, (f ctx) b)
    | Defun(n, a, ty, ll)                   -> Defun(n, a, ty, (f ctx) ll)
    | Switch(cond, cases, s)                -> Switch((f ctx) cond, List.map (fun (ll, s) -> ((f ctx) ll, s)) cases, s)
    | IndrBr(cond, labels)                  -> IndrBr((f ctx) cond, labels)
    | Return(a)                             -> Return((f ctx) a)
    | Branch(cond, s, t)                    -> Branch((f ctx) cond, s, t)
    | ZeroExt(a, t)                         -> ZeroExt((f ctx) a, t)
    | CompatCast(a, t)                      -> CompatCast((f ctx) a, t)
    | Bitcast(a, t)                         -> Bitcast((f ctx) a, t)
    | Alloca(t)                             -> Alloca(t)
    | Load(sub)                             -> Load((f ctx) sub)
    | Store(a, b)                           -> Store((f ctx) a, (f ctx) b)
    | GEP(a, b, offsets)                    -> GEP((f ctx) a, (f ctx) b, offsets)
    | ExtractElem(a, b)                     -> ExtractElem(a, b)
    | InsertElem(a, b, c)                   -> InsertElem((f ctx) a, (f ctx) b, (f ctx) c)
    | ExtractVal(a, xs)                     -> ExtractVal((f ctx) a, xs)
    | InsertVal(a, b, xs)                   -> InsertVal((f ctx) a, (f ctx) b, xs)
    | Suite(xs)                             -> Suite(List.map (f ctx) xs)
    | Locate(l, a)                          -> Locate(l, (f ctx) a)
    | a                                     -> a