module Entry

open LL.Emit
open LL.Exc
open LL.Helper
open LL.IR
open LL.Infras
open LL.Pass
open LL.ControlFlow
open System.IO

let codegen (title : string) (llvm : llvm) : unit =
    let ctx = context.init
    let proc = ref Empty
    let type_table = hashtable()
    let emit' = emit <| type_table <| proc
    try
        emit' ctx <| visit ctx (fun ctx -> elimIfElse ctx >> elimWhile ctx) llvm |> ignore
        System.IO.File.WriteAllText(fmt "../ir-snippets/%s.ll" title, proc.Value.to_ir)
    with LLException(exc) ->
        printfn "test %s failed" title
        printfn "%A" exc

[<EntryPoint>]
let main args =
    let formal_args = [ ("arg1", I 32) ]
    let ret_ty = I 32
    let body = Bin(Add, Get "arg1", Get "arg1")
    let defun = Defun("main", formal_args, ret_ty, body)
    codegen "simple-defun" defun

    let formal_args = [ ("arg1", I 32) ]
    let ret_ty = F 32
    let body = CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32)
    let defun = Defun("main", formal_args, ret_ty, body)

    codegen "test-type-convert" defun
    let formal_args = [ ("arg1", I 32) ]
    let ret_ty = F 32
    let body = Let("var", CompatCast(Bin(Add, Get "arg1", Get "arg1"), F 32), Bin(Add, Get("var"), Const(FD(32, 10.0))))
    let def_test1 = Defun("test1", formal_args, ret_ty, body)
    let formal_args = [ ("arg1", I 32) ]
    let ret_ty = I 64
    let body = CompatCast(App(Get("test1"), [ Get("arg1") ]), I 64)
    let def_test2 = Defun("main", formal_args, ret_ty, body)
    let suite = Suite([ def_test2; def_test1 ])
    codegen "cross-definition" suite
    let formal_args = [ ("arg1", I 32) ]
    let ret_ty = I 32

    let cond =
        Suite([ BlockAddr("main", "truelabel") |> Const
                Bin(Eq, Const <| ID(32, 10L), Get("arg1")) ])

    let branch = Branch(cond, "truelabel", "falselabel")

    let iftrue =
        Suite([ Mark "truelabel"
                Jump "tag" ])

    let iffalse =
        Suite([ Mark "falselabel"
                Store(Get "result", Const <| ID(32, 10L))
                Jump("tag") ])

    let ifresult =
        Let("result", Alloca(I 32),
            Suite([ branch
                    iftrue
                    iffalse
                    Mark "tag"
                    Load(Get "result") ]))

    let ret = Return ifresult
    let whole = Defun("main", formal_args, ret_ty, ret)
    codegen "jump" whole
    let cond1 = Bin(Eq, Const(ID(1, 1L)), Const(ID(1, 0L)))
    let then1 = Const <| ID(32, 123L)
    let else1 = Const <| ID(32, 456L)
    let cond2 = Bin(Eq, IfExp(I 32, cond1, then1, else1), else1)
    let then2 = Const <| ID(32, 111L)
    let else2 = Const <| ID(32, 222L)
    let whole = Defun("main", formal_args, ret_ty, IfExp(I 32, cond2, then2, else2))
    codegen "IfThenElse" whole
    let formal_args = []
    let premire after =
        Let("b", Store(Alloca(I 32), Const(ID(32, 0L))), Let("a", Store(Alloca(I 32), Const(ID(32, 0L))), after))
    let cond = Bin(Lt, Load(Get("a")), Const(ID(32, 123L)))

    let body =
        Suite [ Store(Get("a"), Bin(Add, Load(Get("a")), Const(ID(32, 1L))))
                Store(Get("b"), Bin(Add, Load(Get("b")), Const(ID(32, 2L)))) ]

    let funcBody =
        Return(premire (Suite [ WhileExp(cond, body)
                                Load(Get("a")) ]))

    let whole = Defun("main", formal_args, ret_ty, funcBody)
    codegen "whileTestToDouble" whole
    let ty_def =
        DefTy("master",
              [ I 1
                I 8
                F 32
                Agg([ I 32
                      I 64 ]) ])

    let defun =
        Defun
            ("main", [], I 64,
             ExtractVal
                 (Let("value", Alloca(Alias "master"), Load <| GEP(Get "value", Const <| ID(32, 0L), [])), [ 3; 1 ]))
    codegen "member-accessing" <| Suite [ ty_def; defun ]
    let ty_def =
        DefTy("redyred",
              [ I 32
                I 8 ])

    let defun =
        Defun("main", [], I 32,
              Let("c",
                  Const <| AggD([ ID(32, 10L)
                                  ID(8, 10L) ]), ExtractVal(Get("c"), [ 0 ])))

    codegen "aggregate constant" <| Suite [ ty_def; defun ]

    let defun =
        Defun(
            "main", 
            [], 
            I 32,
            Let("c",
                Lambda(
                    [("e", I 32)], 
                    I 32,
                    Bin(Add, Get("e"), Const <| ID(32, 1L)))
                  ,
                  App(Get("c"), [Const <| ID(32, 2L)])))

    codegen "lambda" <| Suite [ ty_def; defun ]

    let if_exp = 
         IfExp(
            Func([I 32], I 32),
            Bin(Eq, Const <| ID (32, 1L), Const <| ID (32, 1L)), 
            Lambda(
                 [("e", I 32)], 
                  I 32,
                  Bin(Add, Get("e"), Const <| ID(32, 1L))),
            
            Lambda(
                 [("e", I 32)], 
                  I 32,
                  Bin(Add, Get("e"), Const <| ID(32, 2L))))
                
    let defun =
        Defun(
            "main", 
            [], 
            I 32,
            Let("c",
                  if_exp   
                  ,
                  App(Get("c"), [Const <| ID(32, 2L)])))

    codegen "load function pointer" <| Suite [defun]
    0
