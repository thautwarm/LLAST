module LL.Emit
open LL.Infras
open LL.Helper
open LL.Exc
open LL.IR

type 'v arraylist = System.Collections.Generic.List<'v>
let combine a b = Combine(a, b)

let (>>>) (tp: 'a * proc) proc =
    match tp with
    | a, b -> a, combine proc b


let rec emit (types: type_table): context -> llvm -> symbol * proc =



    let rec emit' (ctx: context): (llvm -> symbol * proc)  =

        let assign_tmp (ctx: context) codestr =
            let name = ctx.alloc_name
            name, Ordered <| fmt "%%%s = %s" name codestr

        let convert_routine inst sym dest =
                let codestr =
                   fmt "%s %s to %s"
                       <| inst
                       <| dump_sym sym
                       <| dump_type dest
                let name, proc' = assign_tmp ctx codestr
                {ty=dest; name=Some name; ty_tb=types; is_glob=false}, proc'

        let rec is_int_ext = function
                | Terminator, I _
                | I _, Terminator       -> true
                | I a, I b when a < b -> true
                | Vec(n, l), Vec(n', r) when n = n' -> is_int_ext(l, r)
                | _ -> false

        let compat_cast src dest =
            let ({ty=ty} as sym, proc') = emit' ctx src
            let inst =
                let rec get_inst =
                    function
                    | a, b when a =||= b -> ""
                    | Vec(n, l), Vec(n', r) when n = n' -> get_inst(l, r)
                    | I _, F _   -> "sitofp"
                    | F _, I _   -> "fptosi"
                    | Ptr _, I _ -> "ptrtoint"
                    | I _, Ptr _ -> "inttoptr"
                    | I a, I b when a > b -> "trunc"
                    | I a, I b when a < b -> "sext"
                    | F a, F b when a > b -> "fptrunc"
                    | F a, F b when a < b -> "fpext"
                    | _ -> InvalidUsage(fmt "%s -> %s" <| dump_type ty <| dump_type dest, "convert") |> ll_raise
                in get_inst(ty, dest)
            if inst = "" then sym, proc'
            else
            match convert_routine inst sym dest with
            | sym, proc'' -> sym, combine proc' proc''

        let load_llvm ptr =
            match ptr with
            | {ty = Ptr val_ty} ->
                let name, proc' = load' ptr |> assign_tmp ctx
                {name=Some name; ty_tb=types; is_glob=false; ty=val_ty}, proc'
            | _ ->
            UnexpectedUsage("Pointer type", dump_type ptr.ty, "`load` type") |> ll_raise

        let store_llvm ptr ({ty = ty_of_data} as data) =
            match ptr with
            | {ty=Ptr val_ty} ->
                if val_ty =||= ty_of_data |> not then
                    type_mimatch(ty_of_data, val_ty) |> ll_raise
                else
                ptr, Ordered <| store' data ptr
            | {ty=ty} ->
                failwithf "%A \n %A" ptr ctx.local
                UnexpectedUsage("Pointer type", dump_type ty, "`store` arg type") |> ll_raise

        function
        | Undecided f ->
            let sym = {name = Some <| ctx.alloc_name; ty = Terminator; ty_tb = types; is_glob = false}
            let pending_code() =
                let _, proc' = emit types ctx <| f()
                proc'

            sym, pending pending_code
        | Const constant ->
            let constant, proc' = get_constant types ctx constant
            let proc' =
                match proc' with
                | Empty -> Empty
                | _     ->
                NoIndent proc'

            let ty = match constant.ty with Ptr ty -> ty | _ -> failwith "Impossible"
            match ty with
            | I _
            | F _ -> load_llvm constant >>> proc'
            | _  ->
               let proc' = 
                   try
                      Decl("llvm.memcpy.p0i8.p0i8.i64", [Ptr <| I 8; Ptr <| I 8; I 64; I 32; I 1], Void)
                      |> emit' ctx
                      |> snd
                      |> combine proc'
                   with LLException(UnexpectedUsage(_, _, "declaration")) ->
                      proc'
               let size, align = get_size_and_align types ty
               let ret, proc' = emit' ctx <| Alloca(ty) >>> proc'
               let bitcasted, proc' = convert_routine "bitcast" ret <| Ptr(I 8) >>> proc'
               let codestr =
                   fmt "call void @llvm.memcpy.p0i8.p0i8.i64(%s, i8* bitcast(%s to i8*), i64 %d, i32 %d, i1 false)"
                        <| dump_sym bitcasted
                        <| dump_sym constant
                        <| size
                        <| align
               let proc' = combine proc' <| Ordered codestr
               load_llvm ret >>> proc'

        | Get name ->
            ctx.find <| name, Empty

        | Let(name, value, body) ->
            let value, proc' = emit' ctx value
            let count = ctx.count
            ctx.count <- ctx.count + 1
            let ctx = ctx.into (fmt "%d" count)
            ctx.local.[name] <- value
            emit' ctx body >>> proc'
        | Decl(name, arg_tys, ret_ty) ->
            if ctx.``global``.ContainsKey name then
                UnexpectedUsage(fmt "declare %s" name, "declared once", "declaration") |> ll_raise
            else
            let func_ty = Func(arg_tys, ret_ty)
            let name' = Some name
            ctx.``global``.[name] <- {ty = func_ty; name = name'; ty_tb = types; is_glob = true}

            void_symbol,
            fmt "declare %s %s(%s)"
                <| dump_type ret_ty
                <| actual_name name' true
                <| join (List.map dump_type arg_tys)
                |> Ordered
                |> NoIndent
                |> Predef


        | (Defun(_, args, ret_ty, body) | Lambda(args, ret_ty, body)) as it ->
            let arg_names, arg_tys = List.unzip args
            let func_ty = Func(arg_tys, ret_ty)
            let name, sym =
                match it with
                | Defun(name, _, _, _) ->
                    let sym = {ty=func_ty; name=Some name; ty_tb = types; is_glob=true}
                    ctx.``global``.[name] <- sym
                    name, sym
                | Lambda _ ->
                    let name = ctx.alloc_name
                    name,
                    {ty=func_ty; name=Some name; ty_tb = types; is_glob=true}
                | _ -> failwith "impossible"
            let delay = fun () ->
                let prefix = ctx.wrap_name name
                let arg_syms =
                    List.map
                    <| fun (arg, ty) ->
                        {
                            ty  = ty
                            name = Some <| concat prefix arg
                            ty_tb = types
                            is_glob =false
                        }
                    <| args

                let local_vars = hashtable()

                for each in ctx.local do
                    let key = each.Key
                    let value = each.Value
                    match value.ty with
                    | Func _ ->
                        local_vars.Add(key, value)
                    | _ -> ()

                List.zip arg_names arg_syms
                |>
                List.iter (fun (a, b) -> local_vars.Add(a, b))

                let ctx = ctx.new_from prefix local_vars
                let arg_string = List.map dump_sym arg_syms |> join
                let head =
                    fmt "define %s @%s(%s){"
                    <| dump_type ret_ty
                    <| name
                    <| arg_string
                    |> Ordered

                let body, proc' = emit types ctx body

                let proc' =
                    combine
                    <| proc'
                    <|
                    match body with
                    | {ty=Terminator} -> Empty
                    | _ as sym        ->
                        let ret_stmt = fmt "ret %s" <| dump_sym sym
                        if sym.ty <> ret_ty then
                            Message("function return type conflitcs with end of body") <*> type_mimatch(ret_ty, sym.ty) |> ll_raise
                        else Ordered ret_stmt


                let endl = fmt "}" |> Ordered
                let defun = Indent(proc')
                let defun = Combine(defun, endl)
                let defun = Combine(head, defun)
                NoIndent <| Predef defun
            sym, Pending delay

        | ZeroExt(src, dest) ->
            let sym, proc' = emit' ctx src
            if is_int_ext(sym.ty, dest) then convert_routine "zext" sym dest >>> proc'
            else InvalidUsage(dump_type sym.ty, "zero extending") |> ll_raise
        | Bitcast(src, dest) ->
            let sym, proc' = emit' ctx src
            convert_routine "bitcast" sym dest >>> proc'

        | CompatCast(src, dest) ->
            compat_cast src dest

        | Return value ->
            let proc' =
                match emit' ctx value with
                | {ty=Void}, proc' ->
                    combine proc' <| Ordered "ret void"
                | _ as sym, proc'->
                    fmt "ret %s" <| dump_sym sym |> Ordered
                    |> combine proc'
            terminator, proc'

        | IndrBr(cond, labels) ->
            let cond, proc' = emit' ctx cond
            let cond = dump_sym cond
            let pending_code () =
                let label_string =
                    List.map
                    <| fun label -> dump_sym ctx.local.[label]
                    <| labels
                    |> join
                let codestr = fmt "indirectbr %s, [ %s ]" cond label_string
                Ordered codestr

            terminator, combine proc' <| Pending pending_code

        | Mark(name) ->
            let actual_name = ctx.alloc_name
            let proc' = fmt "%s:" actual_name |> Ordered |> NoIndent
            let label: symbol =
                {
                    name    = Some actual_name
                    ty_tb   = types
                    ty      = Label
                    is_glob = false
                 }
            ctx.local.[name] <- label
            label, proc'
        | Branch(cond, iftrue, iffalse) ->
            let cond, proc' = emit' ctx cond
            if cond.ty =||= I 1 |> not then UnexpectedUsage(dump_type cond.ty, "i1", "branch condition") |> ll_raise
            else
            let cond = dump_sym cond
            let pending_code() =

                let iffalse = dump_sym ctx.local.[iffalse]
                let iftrue = dump_sym ctx.local.[iftrue]
                let codestr = fmt "br %s, %s, %s" cond iftrue iffalse
                Ordered codestr

            terminator, combine proc' <| Pending pending_code

        | Jump(label) ->
            let pending_code() =
                let codestr = fmt "br %s" <| dump_sym ctx.local.[label]
                Ordered codestr
            terminator, Pending pending_code

        | Switch(cond, cases, default') ->
            let cond, proc' = emit' ctx cond
            let cond =  cond |> dump_sym
            let procs, label_pairs =
                List.map
                <| fun (case, label) ->
                    let case, proc' = emit' ctx case
                    let case = dump_sym case

                    proc', fun () -> fmt "%s, %s" case <| dump_sym ctx.local.[label]
                <| cases
                |> List.unzip
            let proc' = List.fold combine proc' procs
            let pending_code() =
                let default' = dump_sym ctx.local.[default']
                let label_pairs =
                    List.map
                    <| fun lazy_get -> lazy_get()
                    <| label_pairs
                    |> join
                fmt "switch %s, %s [ %s ]" cond default' label_pairs |> Ordered

            terminator, pending pending_code |> combine proc'
        | DefTy(name, ty) ->
            types.[name] <- ty
            let defty = fmt "%%.struct.%s = type %s" name <| dump_type ty
            void_symbol, Predef <| Ordered defty
        | Bin(bin_op, lhs, rhs) as bin ->
            let {ty=lty; name = lname; is_glob=l_is_glob}, proc' = emit' ctx lhs
            let {ty=rty; name = rname; is_glob=r_is_glob}, proc' = emit' ctx rhs >>> proc'
            let ty =
                if lty <> rty then
                    Message(fmt "Invalid binary operation(%A)" bin_op)
                    <*>
                    type_mimatch(lty, rty)
                    |> ll_raise
                else
                lty
            let inst, ret_ty =
                let rec get_inst_and_ret_ty =
                    function
                    | Add, (U _ | I _ as ty) -> "add", ty
                    | Add, (F _ as ty) -> "fadd",ty
                    | Sub, (U _ | I _ as ty) -> "sub",ty
                    | Sub, (F _ as ty) -> "fsub",ty
                    | Mul, (U _ | I _ as ty) -> "mul",ty
                    | Mul, (F _ as ty) -> "fmul",ty
                    | Rem, (U _ | I _ as ty) -> "srem",ty
                    | Rem, (F _ as ty) -> "frem",ty
                    | Div, (U _ | I _ as ty) -> "sdiv",ty
                    | Div, (F _ as ty) -> "fdiv",ty

                    | LSh, (U _ | I _ as ty) -> "shl",ty
                    | LShr,(U _ | I _ as ty) -> "lshr",ty
                    | AShr,(I _ as ty) -> "ashr",ty
                    | And, (U _ | I _ as ty) -> "and",ty
                    | Or,  (U _ | I _ as ty) -> "or",ty
                    | XOr, (U _ | I _ as ty) -> "xor",ty
                    // comparator
                    | Eq, U _
                    | Eq, I _ -> "icmp eq" , I 1
                    | Eq, F _ -> "fcmp oeq", I 1

                    | Ne, U _
                    | Ne, I _ -> "icmp ne" , I 1
                    | Ne, F _ -> "fcmp one", I 1

                    | Gt, U _ -> "icmp ugt", I 1
                    | Gt, I _ -> "icmp sgt", I 1
                    | Gt, F _ -> "fcmp ogt", I 1

                    | Ge, U _ -> "icmp uge", I 1
                    | Ge, I _ -> "icmp sge", I 1
                    | Ge, F _ -> "fcmp oge", I 1

                    | Lt, U _ -> "icmp ult", I 1
                    | Lt, I _ -> "icmp slt", I 1
                    | Lt, F _ -> "fcmp olt", I 1

                    | Le, U _ -> "icmp ule", I 1
                    | Le, I _ -> "icmp sle", I 1
                    | Le, F _ -> "fcmp ole", I 1
                    | op, Vec(n, ty) ->
                        match get_inst_and_ret_ty(op, ty) with
                        | inst, ty -> inst, Vec(n, ty)
                    | _ -> InvalidUsage(fmt "%A" bin, "binary operation") |> ll_raise
                get_inst_and_ret_ty(bin_op, lty)
            let lname = actual_name lname l_is_glob
            let rname = actual_name rname r_is_glob
            let codestr = fmt "%s %s %s, %s" inst <| dump_type ty <| lname <| rname
            let name, proc' = assign_tmp ctx codestr >>> proc'
            {ty=ret_ty; name=Some name; is_glob=false; ty_tb=types}, proc'
        | App(func, args) ->
            let emit'' = emit' ctx
            let {ty = fn_ty; name = fn_name; is_glob = is_glob}, proc' = emit'' func

            let args, procs = List.map emit'' args |> List.unzip
            let proc' = List.fold combine proc' procs
            match fn_ty with
            | Func(fn_arg_tys, fn_ret_ty) ->
                if fn_arg_tys <> [for each in args -> each.ty]
                then
                    Message("function input arg types mismatch")
                    <*>
                    type_mimatch(fn_ty, Func([for each in args -> each.ty], fn_ret_ty))
                    |> ll_raise
                else
                let arg_string =
                    List.map dump_sym args
                    |> String.concat ", "
                match fn_ret_ty with
                | Void ->
                    let codestr = fmt "call void %s(%s)" (actual_name fn_name is_glob) arg_string

                    void_symbol, combine proc' <| Ordered codestr
                | _    ->
                let codestr = fmt "call %s %s(%s)" <| dump_type fn_ret_ty <| (actual_name fn_name is_glob) <| arg_string
                let name, proc' = assign_tmp ctx codestr >>> proc'
                {name=Some name; ty = fn_ret_ty; is_glob=false; ty_tb=types}, proc'
            | _ ->
            type_mimatch(Func([PendingTy nil_undecided], PendingTy nil_undecided), fn_ty) |> ll_raise

        | Suite suite ->
            let rec loop = function
                | [expr] ->
                    emit' ctx expr
                |  expr :: suite ->
                    let _, proc' = emit' ctx expr
                    loop suite >>> proc'
                | [] -> void_symbol, Empty
            in loop suite

        | Alloca ty ->
            let ptr = {ty=Ptr ty; name = Some <| ctx.alloc_name; ty_tb=types; is_glob=false}
            let codestr = alloca' ptr
            ptr, Ordered codestr
        | AllocaTo ptr ->
            let codestr = alloca' ptr
            ptr, Ordered codestr
        | Load subject ->
            let sym, proc' = emit' ctx subject
            load_llvm sym >>> proc'

        | Store(subject, data) ->
            let data, proc' = emit' ctx data
            let subject, proc' = emit' ctx subject >>> proc'
            store_llvm subject data >>> proc'

        | GEP(subject, idx, offsets) ->
            let subject, proc' = emit' ctx subject
            let idx, proc' = emit' ctx idx >>> proc'
            match subject with
            | {ty = Ptr ty} as ptr ->
                let idx = dump_sym idx
                let ret_ty = Ptr <| find_ty types offsets ty
                let name, proc' = gep' ptr idx offsets  |> assign_tmp ctx >>> proc'
                {name=Some name; ty = ret_ty; is_glob=false; ty_tb=types}, proc'
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`getlementptr` arg type") |> ll_raise

        | ExtractElem(subject, idx) ->
            let subject, proc' = emit' ctx subject
            let idx, proc'     = emit' ctx idx >>> proc'
            match subject with
            | {ty = Vec(_, ty)} ->
                let name, proc' = extractelem' subject idx |> assign_tmp ctx >>> proc'
                {ty=ty; name = Some name; ty_tb=types; is_glob = false}, proc'
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`extractelement` arg type") |> ll_raise

        | InsertElem(subject, val', idx) ->
            let subject, proc' = emit' ctx subject
            match subject with
            | {ty = Vec(_, ty)} ->
                let val', proc'    = emit'  ctx val' >>> proc'
                let idx, proc'     = emit'  ctx idx  >>> proc'
                if val'.ty =||= ty |> not then
                    Message("`insertelementptr` arg types mismatch") <*> type_mimatch(ty, val'.ty) |> ll_raise
                let name, proc' = insertelem' subject val' idx |> assign_tmp ctx >>> proc'
                {ty = ty; name = Some name; ty_tb = types; is_glob = false}, proc'
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`insertelement` arg type") |> ll_raise


        | ExtractVal(subject, indices) ->
            let subject, proc' = emit' ctx subject
            let ty = find_ty types indices subject.ty
            let name, proc' = extractval' subject indices |> assign_tmp ctx >>> proc'
            {ty = ty; name = Some name; ty_tb = types; is_glob=false}, proc'

        | InsertVal(subject, val', indices) ->
            let subject, proc' = emit' ctx subject
            let ty = find_ty types indices subject.ty
            let val', proc' = emit' ctx val' >>> proc'
            let name, proc' = insertval' subject val' indices |> assign_tmp ctx >>> proc'
            {ty = ty; name = Some name; ty_tb = types; is_glob=false}, proc'

        | DefVar(name, var) ->
            let var, proc' = emit' ctx <| Const var
            ctx.``global``.[name] <- var
            var, proc'
        | Emitted(sym, proc') -> sym, proc'
        | Monitor(f, llvm) ->
            let sym = emit' ctx llvm
            f sym
            sym
        | Rewrite(llvm_array, rewrite_fn) ->
            Array.map <| emit' ctx <| llvm_array
            |> rewrite_fn
            |> emit' ctx

        | Locate(loc, llvm) ->
            try
                emit' ctx llvm
            with LLException(exc) ->
            LocatedExc(exc, loc) |> ll_raise
        | _ as it -> failwithf "%A" it

    emit'
