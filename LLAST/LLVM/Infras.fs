module LLVM.Infras
open LLVM.IR

let fmt               = sprintf
let inline to_str arg = fmt "%s" arg
let inline concat a b = a + "." + b
let inline join lst   = String.concat ", " <| List.map to_str lst

type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>

type type_table = (string,  ``type``) hashtable

type symbol = {
    name    : string option
    ty_tb   : type_table
    ty      : ``type``
    is_glob : bool
}

and context = {
    ``global`` : (string,   symbol) hashtable
    local      : (string,   symbol) hashtable
    mutable count   : int
    prefix          : string (** for context mangling usage *)
}
