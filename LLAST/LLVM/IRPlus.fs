module LLVM.IRPlus
open LLVM.IR

type llvmP = 
    | IR of block : llvm 
    | IfExp of ``type`` *  cond : llvmP * thenBlock : llvmP * elseBlock : llvmP

type context = {
    prefix : string
}


let rec toLLVM (ctx:context) (ast : llvmP) :  llvm = 
    match ast with
    | IR(a) -> a
    | IfExp(ty, cond,thenBlock,elseBlock) -> 
        ifte ctx ast
and ifte ctx lP =
    match lP with 
    | IfExp(ty,cond,thenBlock,elseBlock) ->
        let truelabel = ctx.prefix + ".truelabel"
        let falselabel = ctx.prefix + ".falselabel"
        let endlabel = ctx.prefix + ".endlabel"
        let branch = Branch(toLLVM {prefix=ctx.prefix+".cond"} cond,truelabel,falselabel)
        let trueBlock = toLLVM {prefix=ctx.prefix+".then"} thenBlock
        let falseBlock = toLLVM {prefix=ctx.prefix+".else"} elseBlock
        Let(
            ".result", 
            Alloca(ty, None),
            Suite [
                branch;
                Mark truelabel
                Store (Get(".result"), trueBlock)
                Jump(endlabel);
                Mark(falselabel);
                Store (Get(".result"), falseBlock)
                Mark(endlabel);
                Load(Get(".result"))
              ])

