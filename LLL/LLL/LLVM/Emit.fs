
module Emit

open IR
open System
open System.Net.Http


type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>
type 't arraylist = System.Collections.Generic.List<'t>
type context = {
    ``global``    : (string, ``type``) hashtable;
    local         : (string, ``type``) hashtable;
    mutable count : int;
    identifier    : string (** identifier of context for mangling *)
    }
    with
    member this.alloc_name() =
        let ret = this.count
        this.count <- ret + 1
        sprintf "%s %d" this.identifier ret
    member this.fresh() =
        {this with identifier = this.alloc_name(); count = 0; local = hashtable(this.local)}
    member this.bind name ty = 
        let ref = ref IR.Void 
        match this.local.TryGetValue(name, ref) with 
        | true  -> failwithf "Name %s existed in local scope." name
        | false -> this.local.Add(name, ty)
    member this.find(name) = 
        let ref = ref IR.Void
        match this.local.TryGetValue(name, ref) with 
        | true  -> ref.Value
        | false ->
        match this.``global``.TryGetValue(name, ref) with 
        | true -> ref.Value
        | false -> failwithf "Name %s not exists in both local and global scope." name
    member this.into(name) = 
        {this with identifier = name; count = 0; local = hashtable(this.local)}

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
    | NamedAgg(name, _) -> sprintf "%%struct.%s" name
    | Func(args, ret)   -> sprintf "%s (%s)" <| dump_type ret <| (String.concat ", " <| List.map dump_type args)  
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

let rec get_align_from_ty ty : int = 
    match ty with
    | Void   -> 0
    | Func _ -> 0
    | F bit
    | I bit -> Math.Max(bit/8, 1)
    | Ptr _ -> 8
    | Vec(_, ty) -> 8 + get_align_from_ty ty
    | Arr(_, ty) -> 8 + get_align_from_ty ty
    | Agg lst    ->  List.map get_align_from_ty lst |> List.max
    | NamedAgg(_, asoc_list) -> 
    List.map 
    <| fun (_, ty) -> get_align_from_ty ty
    <| asoc_list 
    |> List.max

    

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

let gep' ty ptr_name idx offsets = 
    let ty = dump_type ty
    sprintf "getelementptr inbounds %s, %s* %s, %s, %s" ty ty ptr_name idx <| String.concat ", " offsets


let emit (cr: compiler) =
    let assign_tmp (context: context) (code: string): string =
        let name = context.alloc_name()
        cr.write <| sprintf "%s = %s" name code
        name
    let rec inner(context: context) (llvm: llvm): symbol =
        let branch_of context label_start body label_end ret_name: ``type`` =
            cr.write_no_indent <| sprintf "%s:" label_start
            let {ty=ty; name=name} = inner context body
            if ty = IR.Void then ()
            else cr.write <| store' ty name ret_name
            cr.write <| sprintf "br label %%%s" label_end
            ty

        match llvm with
        | Sym symbol -> symbol
        | Const constant ->
            let ty, code = typed_data constant
            // nvr Void type here
            {ty = ty; name = assign_tmp context code}
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
                {ty = I 1; name = assign_tmp context code}
            | F bit, F bit' when bit = bit' ->
                let type_str = dump_type (F bit)
                let code = sprintf "fcmp %s %s %s, %s" cmp_cond type_str lname rname
                {ty = I 1; name = assign_tmp context code}
            | Vec(n, elem_ty), Vec(n', elem_ty') when n' = n && elem_ty' = elem_ty ->
                let type_str = dump_type elem_ty
                let code = sprintf "icmp %s %s %s, %s" cmp_cond type_str lname rname
                {ty = Arr(n, I 1); name = assign_tmp context code}
            | _ -> failwithf "invalid comparison for %A %A" lhs rhs

        | Conv conversion ->
            let routine inst sym dest =
                let {ty=ty; name=name} = sym
                let code =
                    sprintf "%s %s %s to %s" inst
                    <| dump_type ty
                    <| name
                    <| dump_type dest
                {ty = dest; name = assign_tmp context code}

            let cond_routine predicate inst src dest =
                let {ty=ty;} as sym = inner context src
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
                let rec get_inst =
                    function 
                    | op, Vec(_, ty) -> get_inst(op, ty)
                    | Add, I _  -> "add"
                    | Add, F _  -> "fadd"
                    | Sub, I _  -> "sub"
                    | Sub, F _  -> "fsub"
                    | Mul, I _  -> "mul"
                    | Mul, F _  -> "fmul"
                    | Rem, I _  -> "srem"
                    | Rem, F _  -> "frem"
                    | Div, I _  -> "sdiv"
                    | Div, F _  -> "fdiv"
                    | LSh, I _  -> "shl"
                    | LShr,I _  -> "lshr"
                    | AShr,I _  -> "ashr"
                    | And, I _  -> "and"
                    | Or,  I _  -> "or"
                    | XOr, I _  -> "xor"
                    | _ -> failwithf "invalid binary operation %A." bin
                get_inst(bin_op, ty)
            let code = sprintf "%s %s %s, %s" inst <| dump_type ty <| l.name <| r.name
            in
            {ty = ty; name = assign_tmp context code}
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
            | Loop(value, cond, body) ->
                let {ty = ty_of_value; name = value} = inner context value
                let context = context.fresh()
                let branch_of = branch_of context
                let ret_name = context.alloc_name()
                let label_setup = context.alloc_name()
                let label_loop = context.alloc_name()
                let label_end = context.alloc_name()
                cr.write <| alloca' ty_of_value (Some value)
                // setup:
                cr.write_no_indent <| sprintf "%%%s" label_setup
                let {ty=ty; name = cond} = inner context cond
                if ty <> I 1 then
                    failwithf "If-Conditional type mismatch, got %A, expected i1." ty
                else
                cr.write <| sprintf "br i1 %s, label %%%s, label %%%s" cond label_loop label_end
                // loop:
                cr.write_no_indent <| sprintf "%%%s" label_loop
                let {ty = ty; name = body} = inner context body
                let exit = 
                    if ty = IR.Void then 
                        fun () -> void_symbol
                    else 
                        cr.write <| store' ty body ret_name
                        fun () ->
                            let name = assign_tmp context <| load' ty ret_name
                            {ty = ty; name = name}
                cr.write <| sprintf "br label %%%s" label_setup
                // end:
                cr.write_no_indent <| sprintf "%%%s" label_end
                exit()

            | Branch(cond, iftrue, iffalse) ->
            let context = context.fresh()
            let branch_of = branch_of context
            let ret_name = context.alloc_name()
            let label_true = context.alloc_name()
            let label_false = context.alloc_name()
            let label_end = context.alloc_name()
            let result_idx = cr.pending()

            let {ty = ty; name = cond} = inner context cond
            if ty <> I 1 then
                failwithf "If-Condition type mismatch, got %A, expected I1." ty
            else
            cr.write <| sprintf "br i1 %s, label %%%s, label %%%s" cond label_true label_false
            let ty_t = branch_of label_true iftrue label_end ret_name
            let ty_f = branch_of label_true iffalse label_end ret_name
            if  ty_t <> ty_f then
                failwithf "If-Else type mismatch %A <> %A." ty_t ty_f
            else
            cr.write_no_indent <| sprintf "%s:" label_end
            if ty_t = IR.Void then void_symbol
            else
            let name = assign_tmp context <| load' ty_t ret_name
            cr.decide result_idx <| alloca' ty_f None
            {ty=ty_t; name = name}
        | Mem mem_acc ->
            match mem_acc with
            | Alloca(ty, Some(data)) ->
                let {ty = ty_of_data; name = name} = inner context data
                if ty <> ty_of_data then
                    failwithf "allocating failed for type mismatch: %A <> %A." ty ty_of_data
                let name = alloca' ty (Some name)
                let ty   = Ptr ty
                context.bind name ty
                {ty = Ptr ty; name = name}
            | Alloca(ty, None) ->
                {ty = Ptr ty; name = alloca' ty None}
            | Load name ->
                match context.find name with
                | Ptr ty ->
                    let name = assign_tmp context <| load' ty name
                    {ty = ty; name = name}
                | _ as ty ->
                failwithf "invalid load instruction, expected Ptr of some type, got type %A." ty
            | Store(name, data) ->
                let {ty = ty_of_data; name=val_name} = inner context data
                match context.find name with
                | Ptr ty ->
                    if ty <> ty_of_data then
                        failwithf "invalid store instruction, %A mismatch %A." ty ty_of_data
                    else
                    cr.write <| store' ty val_name name
                    void_symbol
                | _ as ty -> failwithf "invalid load instruction, expected Ptr of some type, got type %A." ty
            | GEP(name, idx, offsets) ->
                match inner context idx with
                | {ty=idx_ty; name=idx} ->
                match context.find name with
                | Ptr ty ->
                    let idx = sprintf "%s %s" <| dump_type idx_ty <| idx
                    let ret_ty = 
                        let rec find_ty offsets agg_ty =
                            match offsets with
                            | [] -> agg_ty
                            | offset :: offsets ->
                            match agg_ty with
                            | Agg(list) -> find_ty offsets list.[offset]
                            | Vec(n, ty) -> if n < offset then find_ty offsets ty
                                            else failwithf "IndexError."
                            | NamedAgg(_, asoc_list) ->
                                match asoc_list.[offset] with
                                | (_, ty) -> find_ty offsets ty
                            | _ -> failwith "Type error."
                        in find_ty offsets ty
                    let name = assign_tmp context <| (gep' ty name idx <| List.map dump_str offsets)
                    {name=name; ty = ret_ty}
                | _ -> failwith "Invalid instruction getelementptr."
        | Suite suite ->
            let rec loop = function
                | [expr] ->
                    inner context expr
                |  expr :: suite ->
                    inner context expr |> ignore
                    loop suite
                | [] -> void_symbol
            in loop suite
        | Define(name, args, ret, body) ->
            let arg_types, arg_names = List.unzip args
            let func_ty = IR.Func(arg_types, ret)
            let arg_string = 
                List.map 
                <| fun (ty, arg) -> sprintf "%s %s" <| dump_type ty <| arg
                <| args
                |> String.concat ", "
            sprintf "define %s @%s(%s){"
            <| dump_type ret 
            <| name 
            <| arg_string
            |> cr.write
            context.``global``.[name] <- func_ty
            let context = context.into(name)
            match inner context body with
            | {ty = IR.Void;}    -> 
                if ret <> IR.Void then failwith "type mismatch"
                else {name=name; ty = func_ty}
            | {ty=ty; name=name} ->
                if ret = IR.Void then cr.write <| "ret void"
                else if ret =  ty then
                    cr.write <| (sprintf "ret %s %s" <| dump_type ty <| name)
                else failwith "type mismatch"
                {name = name; ty = func_ty}
            |>
            fun fn ->
                cr.write("\n}")
                fn

    in inner
