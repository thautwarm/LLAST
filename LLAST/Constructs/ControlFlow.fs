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
        
        let symbol_ref = ref void_symbol
        let allocate_monitor sym = symbol_ref := sym
        let unify =
            function
            | {ty = ty} when ty = Terminator -> ()
            | {ty = ty} when symbol_ref.Value.ty = Terminator ->
                symbol_ref.Value.ty <- ty
            | {ty = ty} when  symbol_ref.Value.ty = ty -> ()
            | {ty = ty} -> ll_raise <| type_mimatch(ty, symbol_ref.Value.ty)
            
            
        Let(".result",
            Monitor(allocate_monitor, 
                    pending <| fun ()-> 
                    match symbol_ref.Value with
                    | {ty = ty; name = Some name} -> AllocaTo(ty, name)
                    | _ -> failwith "impossible"),
            Suite [ Branch(cond, truelabel, falselabel)
                    Mark truelabel
                    Store(Get(".result"), Monitor(unify, thenBlock))
                    Jump(endlabel)
                    Mark(falselabel)
                    Store(Get(".result"), Monitor(unify, elseBlock))
                    Jump(endlabel)
                    Mark(endlabel)
                    Load(Get(".result")) ])
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
