module LLL.LLVM.IR
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
| Label
| Terminator (** only works when jump appears inner expression.*)
| Void
| PendingTy of ``type`` undecided

type llvm =
| Bin         of binary_operation
(** binary operations including comparisons *)
| App         of llvm * llvm list
(** application *)

| Get         of name: string
(** get local var *)
| Let         of name: string * value: llvm * body: llvm
(** bind local var which create new context*)
| Defun       of name: string * args: ``type`` asoc_list * ret_ty: ``type`` * body : llvm
(** define function **)
| Const       of constant
(** constant data *)

(** conversions: trunc, fptrunc, ..., bitcast *)
| DefTy       of name: string * ``type`` list

(** control flow*)
| Switch      of cond: llvm * cases: (llvm * string) list * default': string
| IndrBr      of cond: llvm * labels: string list
| Return      of llvm
| Mark        of name: string (** make label*)
| Branch      of cond: llvm * iftrue: string * iffalse: string
| Jump        of label: string

(** conversion*)
| ZeroExt     of src: llvm * dest: ``type``    // zext; Ext means extending
| CompatCast  of src: llvm * dest: ``type``
| Bitcast     of src: llvm * dest: ``type``

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
| Alloca      of ``type`` * data: llvm option
| Load        of name: string
| Store       of name: string * data: llvm
| GEP         of name: string * idx : llvm * offsets: int list
| ExtractElem of subject: llvm * val': llvm * idx: int
| InsertElem  of subject: llvm * val': llvm * idx: int
| ExtractVal  of subject: llvm * val': llvm * idx: int
| InsertVal   of subject: llvm * val': llvm * idx: int

(** others *)
| Suite       of llvm list
| PendingLLVM of llvm undecided
(**
switch, indirectbr, return are all terminators and returns Wildcard.

    *)

(** the name must be in current jump area. **)

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
// comparator
| Eq
| Gt
| Ge
| Lt
| Le
| Ne
| PendingBinOp of binary_operator undecided


and binary_operation = binary_operator * llvm * llvm


and constant =
| ID    of bit: int * value: int64
| FD    of bit: int * value: float
| ArrD  of constant list
| VecD  of constant list
| AggD  of constant list
| Undef of ``type``
| PendingConst of constant undecided



