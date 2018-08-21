
module Emit

open IR
open System

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
    member this.write_no_indent(code: string) =
        this.codes.Add <| code
        ()
    member this.pending(): int =
        this.codes.Add("<pending>")
        this.codes.Count
    member this.decide idx code =
        this.codes.[idx] <- code
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

let get_align_from_ty ty : int = raise <| NotImplementedException()
let store' ty val_name ptr_name =
    let ty = dump_type ty
    sprintf "store %s %s, %s* %s" ty val_name ty ptr_name
let alloca' ty maybe_val_name  =
    let align = get_align_from_ty ty
    let ty    = dump_type ty
    match maybe_val_name with
    | None -> sprintf "alloca %s, align %d" ty align
    | Some(val_name) -> sprintf "alloca %s, %s %s, align %d" ty ty val_name align
let load' ty ptr_name =
    let ty = dump_type ty
    sprintf "load %s, %s* %s" ty ty ptr_name
let emit (cr: compiler) =
    let rec inner(context: context) (llvm: llvm): symbol =
        let assign_tmp code =
            let name = context.alloc_name()
            cr.write <| sprintf "%s = %s" name code
            name
        match llvm with
        | Const constant ->
            let ty, code = typed_data constant
            // nvr Void type here
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
        | Bin bin ->
            let bin_op, l, r = bin
            let l = inner context l
            let r = inner context r
            let ty =
                if l.ty <> r.ty then
                    failwithf "type mismatch: %A <> %A" l.ty r.ty
                else
                l.ty
            let inst =
                match bin_op, ty with
                | Add, I _
                | Add, Vec(_, I _) -> "add"
                | Add, F _
                | Add, Vec(_, F _) -> "fadd"
                | Sub, I _
                | Sub, Vec(_, I _) -> "sub"
                | Sub, F _
                | Sub, Vec(_, F _) -> "fsub"
                | Mul, I _
                | Mul, Vec(_, I _) -> "mul"
                | Mul, F _
                | Mul, Vec(_, F _) -> "fmul"
                | Rem, I _
                | Rem, Vec(_, I _) -> "srem"
                | Rem, F _
                | Rem, Vec(_, F _) -> "frem"
                | Div, I _
                | Div, Vec(_, I _) -> "sdiv"
                | Div, F _
                | Div, Vec(_, F _) -> "fdiv"
                | LSh, I _
                | LSh, Vec(_, I _) -> "shl"
                | LShr, I _
                | LShr, Vec(_, I _) -> "lshr"
                | AShr, I _
                | AShr, Vec(_, I _) -> "ashr"
                | And, I _
                | And, Vec(_, I _) -> "and"
                | Or, I _
                | Or, Vec(_, I _) -> "or"
                | XOr, I _
                | XOr, Vec(_, I _) -> "xor"
                | _ -> failwithf "invalid binary operation %A." bin
            let code = sprintf "%s %s %s, %s" inst <| dump_type ty <| l.name <| r.name
            in
            {ty = ty; name = assign_tmp code}
        | Cf control_flow ->
            match control_flow with
            | Return exp ->
                let {ty = ty; name = name} = inner context exp
                match ty with
                | Void ->
                    cr.write "ret void"
                    void_symbol
                | _   ->
                cr.write <| (sprintf "ret %s %s" <| dump_type ty <| name)
                {ty = ty; name=name}
            | Branch(cond, iftrue, iffalse) ->
            let ret_name = context.alloc_name()
            let label_true = context.alloc_name()
            let label_false = context.alloc_name()
            let label_end = context.alloc_name()
            let result_idx = cr.pending()
            
            let {ty = ty; name = cond} = inner context cond
            if ty <> I 1 then 
                failwithf "If-Conditional type mismatch, got %A, expected I1" ty
            else
            cr.write <| sprintf "br i1 %s, label %%%s, label %%%s" cond label_true label_false
            cr.write_no_indent <| sprintf "%s:" label_true
            let {ty=ty_t; name=name_t} = inner context iftrue
            cr.write <| store' ty_t name_t ret_name
            cr.write <| sprintf "br label %%%s" label_end
            
            cr.write_no_indent <| sprintf "%s:" label_false
            let {ty=ty_f; name=name_f} = inner context iffalse
            cr.write <| store' ty_f name_f ret_name
            cr.write <| sprintf "br label %%%s" label_end            
            
            if ty_f <> ty_t then
                failwithf "If-Else type mismatch %A <> %A." ty_f ty_t
            else 
            cr.write_no_indent <| sprintf "%s:" label_end
            let name = assign_tmp <| load' ty_t ret_name
            cr.decide result_idx <| alloca' ty_f None
            {ty=ty_t; name = name}

        | Sym symbol ->
            symbol
        | _ -> failwith ""
    in inner
