module LLL.ASDL
open RBNF.ParserC

type ('t, 'n) SList = 
    | Nil 
    | Cons of 't * 'n

type ``type`` =
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Ptr  of ``type``
    | Arr  of ``type``
    | Vec  of ``type``
    | Agg  of ``type`` list
    | Func of (``type`` list) * ``type`` 

type data =
    | I8D  of int8
    | I16D of int16
    | I32D of int32
    | I64D of int64
    | F32D of float32
    | F64D of float
    | PtrD of data
    // Both Arr and Vec are sized.
    | ArrD of data list
    | VecD of data list
    // llvm called it aggregrate, which maps to `struct` in C/C++.
    | AggD of data list

type typed_data = {
    data : data
    ty   : ``type``
}

type lll =
    | Lit       of data
    | Val       of value
    | Defun     of func
    | Deftype   of ``type``
    | App       of value  list
    | Lam       of func

and value = {
    actual_name : string 
    lexer_name  : string
}

and func = {
    name     : string
    arg_tys  : ``type`` list
    formals  : string list
    ret_ty   : ``type`` 
    body     : lll
}
