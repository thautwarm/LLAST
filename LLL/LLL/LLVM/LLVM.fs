module IR
type 'v asoc_list = (string * 'v) list
type llvm =
| Const  of constant
| Comp   of compare
| Conv   of conversion
| Bin    of binary_operation
| Cf     of control_flow
| Mem    of memory_access
| Sym    of symbol
| Suite  of llvm list
| Define of name: string * args: (``type`` * string) list * ret_ty: ``type`` * body: llvm


and constant =
| ID    of bit: int * value: int64
| FD    of bit: int * value: float
| ArrD  of constant list
| VecD  of constant list
| AggD  of constant list
| Undef of ``type``

and ``type`` =
| I     of bit: int
| F     of bit: int
| Arr   of len: int * ``type``
| Vec   of len: int * ``type``
| Agg   of ``type`` list
| Func  of ``type`` list * ``type``
| NamedAgg of name: string * fields: (``type`` asoc_list)
| Ptr   of ``type``
| Void

and generic_comparator =
| Eq
| Gt
| Ge
| Lt
| Le
| Ne

and compare = generic_comparator * llvm * llvm

//and float_compare =
//// codegen: fcmp + _.tolower()
//| Ordered   of generic_compare
//| JustOrdered // inst:ord
//| Unordered of generic_compare
//| JustUnordered // une

//and integral_compare =
//// codegen: icmp + _.tolower()
//| Unsiged of generic_compare
//| Singed  of generic_compare

//and compare =
//| I of integral_compare * llvm * llvm 
//| F of float_compare    * llvm * llvm

and conversion =
| Truncate   of src: llvm * dest: ``type`` //trunc ... to type
// where type must be one concrete type of integter or floating
| ZeroExt    of src: llvm * dest: ``type``    // zext; Ext means extending
| SignExt    of src: llvm * dest: ``type``    // sext
| FloatTrunc of src: llvm * dest: ``type``    // fptrunc
| FloatExt   of src: llvm * dest: ``type``    // fpext
| Convert    of src: llvm * dest: ``type``
// prospective type pairs of src, dest :
// (float, int) (int, float)
// (ptr, int)  (int, ptr)
| Bitcast    of src: llvm * dest: ``type``

and vector_manipulate =
| ExtractElem of subject: llvm * idx: int
// <result> = extractelement <n x <ty>> <val>, <ty2> <idx>  ; yields <ty>
| InsertElem  of subject: llvm * elt: llvm *idx: int
// <result> = insertelement <n x <ty>> <val>, <ty> <elt>, <ty2> <idx>    ; yields <n x <ty>>
and aggregate_manipulate =
| ExtractVal  of subject: llvm * idx: int
// <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*
| InsertVal   of subject: llvm * elt: llvm * idx: int
// <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type>
and manipulate =
| VecM of vector_manipulate
| AggM of aggregate_manipulate

and memory_access =
| Alloca of ``type`` * data: llvm option
// alloca <ty>, align n
// alloca <ty>, data
// alloca <ty>
| Load   of name: string
// load <ty>, <ty>* <ptr>
| Store  of name: string * data: llvm 
// store <ty> <val>, <ty>* <ptr>
| GEP    of name: string * idx: llvm * offsets: int list
// getelementptr <ty>, <ty>* <ptr>, i32 idx, i32 offset, ...offsets

and control_flow =
| Return of llvm
// ret <ty> data
// ret void
| Loop of value: llvm * cond: llvm * body: llvm
| Branch of cond: llvm * iftrue: llvm * iffalse: llvm
// br i1 <cond>, label <iftrue>, label <iffalse>
// br label <dest>

and binary_operator =
| Add
| Sub
| Mul
| Div
| Rem
| LSh
| LShr
| AShr
| And
| Or
| XOr

and binary_operation = binary_operator * llvm * llvm

and symbol = {
    ty       : ``type``
    name     : string
}

let void_symbol = {ty = Void; name = "<void>"}

