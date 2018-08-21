
module LLVM


type LLVM =
    | LLVM

and
and constant =
    | ID  of bit:int * value:int64
    | FD  of bit:int * value:float64
    | Arr of constant list
    | Vec of constant list
    | Agg of constant list
    | Undef
    | VoidD

and ``type`` =
    | I of bit:int
    | F if bit:int
    | Void
    | Ptr of ``type``

and generic_compare =
    | Eq
    | Gt
    | Ge
    | Lt
    | Le
    | Ne

and float_compare =
    // codegen: fcmp + _.tolower()
    | Ordered   of generic_compare
    | JustOrdered // inst:ord
    | Unordered of generic_compare
    | JustUnordered // une

and integral_compare =
    // codegen: icmp + _.tolower()
    | Unsiged of generic_compare
    | Singed  of generic_compare

and compare =
    | I of Integral
    | F of Floating

and conversion =
    | Truncate   of desc: ``type`` //trunc ... to type
    // where type must be one concrete type of integter or floating
    | ZeroExt    of dest: ``type``    // zext; Ext means extending
    | SignExt    of desc: ``type``    // sext
    | FloatTrunc of desc: ``type``    // fptrunc
    | FloatExt   of desc: ``type``    // fpext

    | Convert    of src: ``type`` * desc : ``type``
    // prospective type pairs of src, desc :
    // (float, int) (int, float)
    // (ptr, int)  (int, ptr)
    | Bitcast    of desc: ``type``

and vector_manipulate =
    | ExtractElem of idx: int
    // <result> = extractelement <n x <ty>> <val>, <ty2> <idx>  ; yields <ty>
    | InsertElem  of idx: int
    // <result> = insertelement <n x <ty>> <val>, <ty> <elt>, <ty2> <idx>    ; yields <n x <ty>>
and aggregate_manipulate =
    | ExtractVal  of idx: int
    // <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*
    | InsertVal   of idx: int
    // <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type>


and memory_access =
    | Alloca of ``type`` * align: int
    // alloca <ty>, align n
    // alloca <ty>, data
    // alloca <ty>
    | Load   of ``type``
    // load <ty>, <ty>* <ptr>
    | Store  of ``type``
    // store <ty>, <ty>* <ptr>
    | GEP    of ``type``
    // getelementptr <ty>, <ty>* <ptr>, i32 idx, i32 offset, ...offsets

and terminator =
    | Return of ``type``
    // ret <ty> data
    // ret void
    | Branch of string
    // br i1 <cond>, label <iftrue>, label <iffalse>
    // br label <dest>
