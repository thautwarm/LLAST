module LLL.LLVM.ML
(** 
Meta language constructs which could be emitted to LLVM IR.
*)

type 'v asoc_list = (string * 'v) list
type ``type`` =
| I        of bit: int
| F        of bit: int
| Arr      of len: int * ``type``
| Vec      of len: int * ``type``
| Agg      of ``type`` list
| Func     of ``type`` list * ``type``
| Ptr      of ``type``
| NamedAgg of name: string
| Void


type llvm =
| Bin of binary_operation
(** binary operations including comparisons *)
| App of llvm * llvm list
(** application *)
| Acc of accessing
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
| Get of name: string
(** get local var *)
| Let of name: string * value: llvm * body: llvm
(** bind local var which create new context*)
| Defun of name: string * args: ``type`` asoc_list * ret_ty: ``type`` * body : llvm
(** define function **)
| Const
(** constant data *)
| Conv  of conversion
(** conversions: trunc, fptrunc, ..., bitcast *)
| IfExp of cond: llvm * iftrue: llvm * iffalse: llvm
(** if-exp construct *)
| Loop  of cond: llvm * body: llvm
(** loop construct*)
| DefTy of name: string * ``type`` asoc_list


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

and comparator =
| Eq
| Gt
| Ge
| Lt
| Le
| Ne

and binary_operation =
| Comp   of comparator * llvm * llvm
| Normal of binary_operator * llvm * llvm

and conversion =
// where type must be one concrete type of integter or floating
| ZeroExt    of src: llvm * dest: ``type``    // zext; Ext means extending
| TypeCast   of src: llvm * dest: ``type``
| Bitcast    of src: llvm * dest: ``type``


and accessing =
| Alloca      of ``type`` * data: llvm option
| Load        of name: string
| Store       of name: string * data: llvm
| GEP         of name: string * idx : llvm * offsets: int list
| ExtractElem of subject: llvm * idx: int
| InsertElem  of subject: llvm * idx: int
| ExtractVal  of subject: llvm * idx: int
| InsertVal   of subject: llvm * idx: int
