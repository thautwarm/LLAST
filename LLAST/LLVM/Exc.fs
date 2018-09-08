module LL.Exc

open LL.IR
open LL.Infras

type exception_type =
    | InvalidUsage of subject_repr : string * target_repr : string
    | UnexpectedUsage of subject_repr : string * expected_repr : string * target_repr : string
    | NameNotFound of name : string * ctx : context
    | LocatedExc of exception_type * location
    | ComposedExc of exception_type * exception_type
    | NullExc of System.Type
    | Message of string
    | NotDecidedYet of obj

(** low level exception:) *)
exception LLException of exception_type

let inline ll_raise exc = raise <| LLException exc
let (<*>) exc1 exc2 = ComposedExc(exc1, exc2)
