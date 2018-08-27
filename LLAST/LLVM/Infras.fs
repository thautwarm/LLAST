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
    consts     : (constant, symbol) hashtable
    mutable count   : int
    prefix          : string (** for context mangling usage *)
}

type proc =
    | Ordered   of string
    | Predef    of proc
    | Combine   of proc * proc
    | Pending   of (unit -> proc)
    | NoIndent  of proc
    | Indent    of proc
    | Empty

type proc with
    member this.to_ir =
        let dump_code_lst = List.rev >> (String.concat "\n")
        let rec to_ir (n: int) (ordered: string list) (predef: string list) =
            function
            | Ordered it -> ((String.replicate n " ") + it) :: ordered, predef
            | Pending it -> to_ir n ordered predef <| it()
            | Predef  it ->
                let ordered', predef' = to_ir n [] [] it
                let predef =
                    if List.isEmpty predef' then predef
                    else List.append predef predef'
                ordered, List.append ordered' predef
            | Combine(l, r) ->
                let ordered, predef = to_ir n ordered predef l
                to_ir n ordered predef r
            | NoIndent p ->
                to_ir 0 ordered predef p
            | Indent   p ->
                to_ir <| n + 1 <| ordered <| predef <| p
            | Empty      ->
                ordered, predef
        let ordered, predef = to_ir 0 [] [] this
        fmt "%s\n%s"
        <| dump_code_lst predef
        <| dump_code_lst ordered