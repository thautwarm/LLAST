module LLL.LLVM.ML
(**
Meta language constructs which could be emitted to LLVM IR.
*)

type 'a undecided = {
    id: int
}

type 'v asoc_list = (string * 'v) list
type ``type`` =
| I     of bit: int
| F     of bit: int
| Arr   of len: int * ``type``
| Vec   of len: int * ``type``
| Agg   of ``type`` list
| Func  of ``type`` list * ``type``
| Ptr   of ``type``
| Alias of name: string
| Label of ret_ty: ``type``
| Void
| PendingTy of ``type`` undecided

type llvm =
| Bin         of binary_operation
(** binary operations including comparisons *)
| App         of llvm * llvm list
(** application *)
| Acc         of accessing
(** data accessing and manipulations *)
(**
including: load, store, alloca, getelementptr
           for agg:
            extractvalue
            insertvalue
           for vec:
            extractelement
            insertelement
*)
| Get         of name: string
(** get local var *)
| Let         of name: string * value: llvm * body: llvm
(** bind local var which create new context*)
| Defun       of name: string * args: ``type`` asoc_list * ret_ty: ``type`` * body : llvm
(** define function **)
| Const       of constant
(** constant data *)
| Conv        of conversion
(** conversions: trunc, fptrunc, ..., bitcast *)
| Switch      of cond: llvm * cases: (llvm * llvm) list
| JumpArea    of (string * llvm) list
| Return      of llvm
(**
   where the size of list must be more than 2.
   the area could have a return value.
*)
| Jump        of name: string (** the name must be in current jump area. **)
| DefTy       of name: string * ``type`` asoc_list
| PendingLLVM of llvm undecided


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
| PendingBinOp of binary_operator undecided

and comparator =
| Eq
| Gt
| Ge
| Lt
| Le
| Ne
| PendingCmp of comparator undecided

and binary_operation =
| Comp       of comparator * llvm * llvm
| Normal     of binary_operator * llvm * llvm
| PendingBin of binary_operation undecided

and conversion =
// where type must be one concrete type of integter or floating
| ZeroExt     of src: llvm * dest: ``type``    // zext; Ext means extending
| TypeCast    of src: llvm * dest: ``type``
| Bitcast     of src: llvm * dest: ``type``
| PendingConv of conversion undecided

and accessing =
| Alloca      of ``type`` * data: llvm option
| Load        of name: string
| Store       of name: string * data: llvm
| GEP         of name: string * idx : llvm * offsets: int list
| ExtractElem of subject: llvm * val': llvm * idx: int
| InsertElem  of subject: llvm * val': llvm * idx: int
| ExtractVal  of subject: llvm * val': llvm * idx: int
| InsertVal   of subject: llvm * val': llvm * idx: int
| PendingAcc  of accessing undecided

and constant =
| ID   of bit: int * value: int64
| FD   of bit: int * value: float
| ArrD of constant list
| VecD of constant list
| AggD of constant list
| Undef
| PendingConst of constant undecided

type symbol = {
    lexer_name  : string
    actual_name : string
    ty          : ``type``
}
