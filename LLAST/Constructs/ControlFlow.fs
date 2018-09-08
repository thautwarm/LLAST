module LL.ControlFlow

open LL.IR
open LL.Infras

let elimIfElse ctx =
    function
    | IfExp(ty, cond, thenBlock, elseBlock) ->
        let truelabel = "truelabel"
        let falselabel = "falselabel"
        let endlabel = "endlabel"
        Let(".result", Alloca(ty),
            Suite [ Branch(cond, truelabel, falselabel)
                    Mark truelabel
                    Store(Get(".result"), thenBlock)
                    Jump(endlabel)
                    Mark(falselabel)
                    Store(Get(".result"), elseBlock)
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
