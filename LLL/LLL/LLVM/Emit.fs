module Emit

open IR

type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>
type 't arraylist = System.Collections.Generic.List<'t>
type context = {
    ``global``    : (string, ``type``) hashtable;
    local         : (string, ``type``) Map;
    mutable count : int;
    identifier    : string (** identifier of context for mangling *)
    }
    with
    member this.alloc_name() =
        let ret = this.count
        this.count <- ret + 1
        sprintf "%s %d" this.identifier ret

type compiler = {
    codes : string arraylist;
    mutable indent : int;
    }
    with
    member this.write(code: string) =
        this.codes.Add <| System.String(' ', this.indent) + code
        ()

let rec dump_type : ``type`` -> string =
    function
    | I bit   -> sprintf "i%d" bit
    | F 32    -> "float"
    | F 64    -> "double"
    | Vec(n, ty) -> sprintf "< %d x %s >" <| n <| dump_type ty
    | Agg ty_lst -> sprintf "{ %s }" <| String.concat " ,"  (List.map dump_type ty_lst)
    | Ptr ty  -> sprintf "%s *" <| dump_type ty
    | Void    -> "void"
    | _ as it  -> failwith <| sprintf "unsupported type %A" it

let dump_str a = sprintf "%A" a

let rec typed_data : constant -> (``type`` * string) = function
    | ID(bit, value) -> I bit, dump_str value
    | FD(32,  value) -> F 32,  dump_str value
    | FD(64,  value) -> F 64,  dump_str value
    | ArrD lst       ->
        let (typed_lst, data_lst) = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            Arr(length, ty),
            sprintf "[ %s ]" <| String.concat " ," data_lst
        | _    -> failwith "element types mismatch"
    | VecD lst      ->
        let (typed_lst, data_lst) = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            Arr(length, ty),
            sprintf "< %s >" <| String.concat " ," data_lst
        | _    -> failwith "element types mismatch"
    | AggD lst ->
        let (type_lst, data_lst) = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        let agg_ty = Agg(type_lst)
        agg_ty,
        sprintf "{ %s }" <| String.concat " ," data_lst

    | Undef ty ->
        ty, "undef"
    | _ as it  -> failwith <| sprintf "unsupported data %A" it

let emit (cr: compiler) =
    let recur inner(context: context) (llvm: llvm) =
        let assign_tmp code =
            let name = context.alloc_name()
            cr.write <| sprintf "%s = %s" name code
            name
        match llvm with
        | Const constant ->
            let ty, code = typed_data constant
            let name = context.alloc_name()
            assign_tmp name code
            {ty = ty; name = name}
        | Comp(comparator, lhs, rhs) ->
            let lty, lcode = inner context lhs
            let rty, rcode = inner context rhs
            let cmp_cond =
                match lhs, comparator with
                | Eq, I _ -> "eq"
                | Eq, F _ -> "oeq"
                | Ne, I _ -> "ne"
                | Ne, F _ -> "one"
                | Gt, I _ -> "sgt"
                | Gt, F _ -> "ogt"
                | Ge, I _ -> "sge"
                | Ge, F _ -> "oge"
                | Lt, I _ -> "slt"
                | Lt, F _ -> "olt"
                | Le, I _ -> "sle"
                | Le, F _ -> "ole"
            match lty, rty with
            | I bit, I bit ->
                let type_str = dump_type (I bit)
                let code = sprintf "icmp %s %s %s, %s" cmp_cond type_str lcode rcode
                {ty = I 1; name = assign_tmp code}
            | F bit, F bit ->
                let type_str = dump_type (F bit)
                let code = sprintf "fcmp %s %s %s, %s" cmp_cond type_str lcode rcode
                {ty = I 1; name = assign_tmp code}
            | Vec(n, elem_ty), Vec(n, elem_ty) ->
                let type_str = dump_type (I bit)
                let code = sprintf "icmp %s %s %s, %s" cmp_cond type_str lcode rcode
                {ty = Arr(n, I 1); name = assign_tmp code}
            | _ -> failwithf "invalid comparison for %A %A" lhs rhs
        | Sym symbol ->
            symbol
        | _ -> failwith ""
