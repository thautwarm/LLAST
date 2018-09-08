module LL.ControlFlow

open LL.IR
open LL.Infras
open LL.Helper
open LL.Exc

let elimIfElse ctx =
    function
    | IfExp(cond, thenBlock, elseBlock) ->
        let truelabel = "truelabel"
        let falselabel = "falselabel"
        let endlabel = "endlabel"
        let sym_ref = ref void_symbol
        let unify (sym, _) =
            match sym with
            | {ty = ty} when ty = Terminator -> ()
            | {ty = ty} when sym_ref.Value.ty = Terminator ->
                sym_ref.Value.ty <- ty
            | {ty = ty} when sym_ref.Value.ty = ty ->
                ()
            | {ty = ty} -> ll_raise <| type_mimatch(ty, sym_ref.Value.ty)
        let allocate_monitor(sym, _) = sym_ref := sym

        rewrite
        <| 
        [|  
            Monitor(allocate_monitor,  pending <| fun ()-> AllocaTo sym_ref.Value)

            cond

            Let(".result", Monitor(unify, thenBlock), Get(".result"))

            Let(".result", Monitor(unify, elseBlock), Get(".result"))
            
        |]
        <|
        function
        | [|alloca; cond; iftrue; iffalse|] -> 
            sym_ref.Value.ty <- Ptr sym_ref.Value.ty
            Let(".result", Emitted alloca,
                Suite
                <|
                [ 
                   Branch(Emitted cond, truelabel, falselabel)
                   Mark truelabel
                   Store(Get ".result", Emitted iftrue)
                   Jump endlabel
                   Mark falselabel
                   Store(Get ".result", Emitted iffalse)
                   Jump endlabel
                   Mark endlabel
                   Load <| Get ".result"
                ])
        
        | _ -> failwith "impossible"
    | a -> a

let elimWhile ctx =
    function
    | WhileExp(cond, body) ->
        let beginLabel = "beginWhile"
        let beginBodyLabel = "beginWhileBody"
        let endlabel = "endWhile"
        Suite [ Jump(beginLabel)
                Mark(beginLabel)
                Branch(cond, beginBodyLabel, endlabel)
                Mark beginBodyLabel
                body
                Jump(beginLabel)
                Mark endlabel ]
    | a -> a
