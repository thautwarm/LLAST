
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
    let rec inner(context: context) (llvm: llvm): symbol =
        let assign_tmp code =
            let name = context.alloc_name()
            cr.write <| sprintf "%s = %s" name code
            name
        match llvm with
        | Const constant ->
            let ty, code = typed_data constant
            {ty = ty; name = assign_tmp code}
        | Comp(comparator, lhs, rhs) ->
            let {ty=lty; name = lname} = inner context lhs
            let {ty=rty; name = rname} = inner context rhs
            let cmp_cond =
                let rec get_inst =
                    function
                    | op, Vec(_, ty) -> get_inst(op, ty)
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
                    | _, (_ as t) -> failwithf "invalid comparison on %A" t
                get_inst(comparator, lty)

            match lty, rty with
            | I bit, I bit' when bit = bit' ->
                let type_str = dump_type (I bit)
                let code = sprintf "icmp %s %s %s, %s" cmp_cond type_str lname rname
                {ty = I 1; name = assign_tmp code}
            | F bit, F bit' when bit = bit' ->
                let type_str = dump_type (F bit)
                let code = sprintf "fcmp %s %s %s, %s" cmp_cond type_str lname rname
                {ty = I 1; name = assign_tmp code}
            | Vec(n, elem_ty), Vec(n', elem_ty') when n' = n && elem_ty' = elem_ty ->
                let type_str = dump_type elem_ty
                let code = sprintf "icmp %s %s %s, %s" cmp_cond type_str lname rname
                {ty = Arr(n, I 1); name = assign_tmp code}
            | _ -> failwithf "invalid comparison for %A %A" lhs rhs
        | Conv conversion ->
            let routine inst sym dest =
                let {ty=ty; name=name} = sym
                let code =
                    sprintf "%s %s %s to %s" inst
                    <| dump_type ty
                    <| name
                    <| dump_type dest
                {ty = dest; name = assign_tmp code}

            let cond_routine predicate inst src dest =
                let {ty=ty; name=name} as sym = inner context src
                if not <| predicate(ty, dest) then failwithf "invalid trunc %A -> %A" src dest
                else routine inst sym dest

            let is_int = function
                | I _, I _ -> true
                | Vec(n, I _), Vec(n', I _) when n = n' -> true
                | _ -> false

            let is_float = function
                | F _, F _ -> true
                | Vec(n, F _), Vec(n', F _) when n = n' -> true
                | _ -> false

            let is_ptr_int_conv = function
                | Vec(n, Ptr _), Vec(n', I _) when n = n' -> true
                | Vec(n, I _), Vec(n', Ptr _) when n = n' -> true
                | Ptr _, I _
                | I   _, Ptr _ -> true
                | _ -> false

            let convert src dest =
                let {ty=ty} as sym = inner context src
                let inst =
                    match ty, dest with
                    | I _, F _ -> "sitofp"
                    | Vec(n, I _), Vec(n', F _) when n = n' -> "sitofp"
                    | F _, I _ -> "fptosi"
                    | Vec(n, F _), Vec(n', I _) when n = n' -> "fptosi"
                    | Ptr _, I _ -> "ptrtoint"
                    | Vec(n, Ptr _), Vec(n', I _) when n = n' -> "ptrtoint"
                    | I _, Ptr _ -> "inttoptr"
                    | Vec(n, I _), Vec(n', Ptr _) when n = n' -> "inttoptr"
                    | a, b when a = b -> ""
                    | _ -> failwithf "invalid convert %A -> %A" ty dest
                if inst = "" then sym  // if types of two sides are the same, do nothing
                else
                routine inst sym dest

            match conversion with
            | Truncate(src, dest)   -> cond_routine is_int "trunc" src dest
            | ZeroExt(src, dest)    -> cond_routine is_int "zext"  src dest
            | SignExt(src, dest)    -> cond_routine is_int "sext"  src dest

            | FloatTrunc(src, dest) -> cond_routine is_float "fptrunc" src dest
            | FloatExt(src, dest)   -> cond_routine is_float "fpext" src dest

            | Bitcast(src, dest)    -> cond_routine is_int  "bitcast" src dest

            | Convert(src, dest)    -> convert src dest
        | Sym symbol ->
            symbol
        | _ -> failwith ""
    in inner
