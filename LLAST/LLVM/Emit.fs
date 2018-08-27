module LLVM.Emit
open LLVM.IR
open LLVM.Infras
open LLVM.Helper
open LLVM.Exc

type 'v arraylist = System.Collections.Generic.List<'v>

let rec emit (types: type_table) (proc: ref<proc>) =
    let combine b =
        proc.contents <- Combine(proc.contents, b)

    let rec emit' (ctx: context): (llvm -> symbol)  =

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
                combine proc'
                {ty=dest; name=Some name; ty_tb=types; is_glob=false}

        let rec is_int_ext = function
                | Terminator, I _
                | I _, Terminator       -> true
                | I a, I b when a < b -> true
                | Vec(n, l), Vec(n', r) when n = n' -> is_int_ext(l, r)
                | _ -> false

        let compat_cast src dest =
            let {ty=ty} as sym = emit' ctx src
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
            if inst = "" then sym
            else
            convert_routine inst sym dest

        let load_llvm ptr = 
            match ptr with
            | {ty = Ptr val_ty} ->
                let name, proc' = load' ptr |> assign_tmp ctx
                combine proc'
                {name=Some name; ty_tb=types; is_glob=false; ty=val_ty}
            | _ ->
            UnexpectedUsage("Pointer type", dump_type ptr.ty, "`load` type") |> ll_raise
        
        let store_llvm (ptr) ({ty = ty_of_data} as data) = 
            match ptr with
            | {ty=Ptr val_ty} ->
                if val_ty <> ty_of_data then
                    UnexpectedUsage(dump_type val_ty, dump_type ty_of_data, "`store` arg type") |> ll_raise
                else
                Ordered <| store' data ptr |> combine
                void_symbol
            | {ty=ty} -> UnexpectedUsage("Pointer type", dump_type ty, "`store` arg type") |> ll_raise

        function
        | PendingLLVM _ as id -> NotDecidedYet(id) |> ll_raise
        | Const constant ->
            let constant, proc' = get_constant types ctx constant
            
            match proc' with
            | Empty -> 
                ()
            | _     ->
                combine proc'
            let ty = match constant.ty with Ptr ty -> ty | _ -> failwith "Impossible"
            match ty with 
            | I _
            | F _ -> 
               
               load_llvm constant 
            | _  ->
               try 
                  Decl("llvm.memcpy.p0i8.p0i8.i64", [Ptr <| I 8; Ptr <| I 8; I 64; I 32; I 1], Void)
                  |> emit' ctx
                  |> ignore
               with LLException(UnexpectedUsage(_, _, "declaration")) ->
                  ()
               let size, align = get_size_and_align types ty
               let ret = emit' ctx <| Alloca(ty, None)
               let bitcasted = convert_routine "bitcast" ret <| Ptr(I 8)
               let codestr = 
                   fmt "call void @llvm.memcpy.p0i8.p0i8.i64(%s, i8* bitcast(%s to i8*), i64 %d, i32 %d, i1 false)"
                        <| dump_sym bitcasted
                        <| dump_sym constant
                        <| size
                        <| align
               combine <| Ordered codestr
               load_llvm ret
        | Get name ->
            ctx.find <| name

        | Let(name, value, body) ->
            let value = emit' ctx value
            let count = ctx.count
            ctx.count <- ctx.count + 1
            let ctx = ctx.into (fmt "%s$%d" name count)
            ctx.local.[name] <- value
            emit' ctx body
        | Decl(name, arg_tys, ret_ty) ->
            if ctx.``global``.ContainsKey name then 
                UnexpectedUsage(fmt "declare %s" name, "declared once", "declaration") |> ll_raise
            else
            let func_ty = Func(arg_tys, ret_ty)
            let name' = Some name
            ctx.``global``.[name] <- {ty = func_ty; name = name'; ty_tb = types; is_glob = true}
            fmt "declare %s %s(%s)" 
                <| dump_type ret_ty 
                <| actual_name name' true 
                <| join (List.map dump_type arg_tys)
                |> Ordered 
                |> Predef 
                |> combine
            void_symbol

        | Defun(name, args, ret_ty, body) ->
            let ctx = ctx.into name
            let arg_names, arg_tys = List.unzip args
            let func_ty = Func(arg_tys, ret_ty)
            ctx.``global``.[name] <- {ty=func_ty; name=Some name; ty_tb = types; is_glob=true}
            let delay = fun () ->
                let arg_syms =
                    List.map
                    <| fun (arg, ty) -> {ty = ty; name=Some <| ctx.wrap_name arg; ty_tb = types; is_glob=false}
                    <| args

                let rec update_map = function
                    | [] -> ()
                    | (arg_name, arg_sym) :: tail ->
                    ctx.bind arg_name arg_sym
                    update_map tail
                in update_map <| List.zip arg_names arg_syms
                let arg_string = List.map dump_sym arg_syms |> join

                let head =
                    fmt "define %s @%s(%s){"
                    <| dump_type ret_ty
                    <| name
                    <| arg_string
                    |> Ordered
                let inner_proc = ref Empty
                let body =
                    emit types inner_proc ctx body

                match body with
                | {ty=Terminator} -> ()
                | _ as sym        ->
                    let ret_stmt = fmt "ret %s" <| dump_sym sym
                    if sym.ty <> ret_ty then
                        Message("function return type conflitcs with end of body") <*> type_mimatch(ret_ty, sym.ty) |> ll_raise
                    else
                    inner_proc.contents <- Combine(inner_proc.contents, Ordered ret_stmt)
                let endl = fmt "}" |> Ordered
                let defun = Indent(inner_proc.Value)
                let defun = Combine(defun, endl)
                let defun = Combine(head, defun)
                NoIndent <| Predef defun
            combine <| Pending delay
            void_symbol
        | ZeroExt(src, dest) ->
            let sym = emit' ctx src
            if is_int_ext(sym.ty, dest) then convert_routine "zext" sym dest
            else InvalidUsage(dump_type sym.ty, "zero extending") |> ll_raise
        | Bitcast(src, dest) ->
            let sym = emit' ctx src
            convert_routine "bitcast" sym dest

        | CompatCast(src, dest) ->
            compat_cast src dest

        | Return value ->
            match emit' ctx value with
            | {ty=Void}->
                Ordered "ret void"
            | _ as sym->
                fmt "ret %s" <| dump_sym sym |> Ordered
            |> combine
            terminator
        | IndrBr(cond, labels) ->
            let cond = emit' ctx cond |> dump_sym
            let label_string =
                List.map
                <| fun label -> fmt "label %s" label
                <| labels
                |> join
            let codestr = fmt "indirectbr %s, [ %s ]" cond label_string
            combine <| Ordered codestr
            terminator
        | Mark(name) ->
            fmt "%s:" name |> Ordered |> NoIndent |> combine
            terminator
        | Branch(cond, iffalse, iftrue) ->
            let cond = emit' ctx cond
            if cond.ty =||= I 1 |> not then UnexpectedUsage(dump_type cond.ty, "i1", "branch condition") |> ll_raise
            else
            let cond = dump_sym cond
            let codestr = fmt "br %s, label %%%s, label %%%s" cond iftrue iffalse
            combine <| Ordered codestr
            terminator
        | Jump(label) ->
            Ordered <| fmt "br label %%%s" label |> combine
            terminator
        | Switch(cond, cases, default') ->
            let cond = emit' ctx cond |> dump_sym
            let label_pairs =
                List.map
                <| fun (case, label) ->
                    let case = emit' ctx case |> dump_sym
                    fmt "%s, label %%%s" case label
                <| cases
                |> join
            fmt "switch %s, %%%s [ %s ]" cond default' label_pairs |> Ordered |> combine
            terminator
        | DefTy(name, ty_lst) ->
            let ty = Agg(ty_lst)
            types.[name] <- ty
            let defty = fmt "%%.struct.%s = type %s" name <| dump_type ty
            Predef <| Ordered defty |> combine
            void_symbol
        | Bin(bin_op, lhs, rhs) as bin ->
            let {ty=lty; name = lname; is_glob=l_is_glob} = emit' ctx lhs
            let {ty=rty; name = rname; is_glob=r_is_glob} = emit' ctx rhs
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
                    | Add, (I _ as ty) -> "add", ty
                    | Add, (F _ as ty) -> "fadd",ty
                    | Sub, (I _ as ty) -> "sub",ty
                    | Sub, (F _ as ty) -> "fsub",ty
                    | Mul, (I _ as ty) -> "mul",ty
                    | Mul, (F _ as ty) -> "fmul",ty
                    | Rem, (I _ as ty) -> "srem",ty
                    | Rem, (F _ as ty) -> "frem",ty
                    | Div, (I _ as ty) -> "sdiv",ty
                    | Div, (F _ as ty) -> "fdiv",ty
                    | LSh, (I _ as ty) -> "shl",ty
                    | LShr,(I _ as ty) -> "lshr",ty
                    | AShr,(I _ as ty) -> "ashr",ty
                    | And, (I _ as ty) -> "and",ty
                    | Or,  (I _ as ty) -> "or",ty
                    | XOr, (I _ as ty) -> "xor",ty
                    // comparator
                    | Eq, I _ -> "icmp eq" , I 1
                    | Eq, F _ -> "fcmp oeq", I 1
                    | Ne, I _ -> "icmp ne" , I 1
                    | Ne, F _ -> "fcmp one", I 1
                    | Gt, I _ -> "icmp sgt", I 1
                    | Gt, F _ -> "fcmp ogt", I 1
                    | Ge, I _ -> "icmp sge", I 1
                    | Ge, F _ -> "fcmp oge", I 1
                    | Lt, I _ -> "icmp slt", I 1
                    | Lt, F _ -> "fcmp olt", I 1
                    | Le, I _ -> "icmp sle", I 1
                    | Le, F _ -> "fcmp ole", I 1
                    | op, Vec(n, ty) ->
                        match get_inst_and_ret_ty(op, ty) with
                        | inst, ty -> inst, Vec(n, ty)
                    | _ -> InvalidUsage(fmt "%A" bin, "binary operation") |> ll_raise
                get_inst_and_ret_ty(bin_op, lty)
            let lname = actual_name lname l_is_glob
            let rname = actual_name rname r_is_glob
            let codestr    = fmt "%s %s %s, %s" inst <| dump_type ty <| lname <| rname
            let name, proc' = assign_tmp ctx codestr
            combine proc'
            {ty=ret_ty; name=Some name; is_glob=false; ty_tb=types}
        | App(func, args) ->
            let emit'' = emit' ctx
            let {ty = fn_ty; name = fn_name; is_glob = is_glob} = emit'' func

            let args = List.map emit'' args
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
                    combine <| Ordered codestr
                    void_symbol

                | _    ->
                let codestr = fmt "call %s %s(%s)" <| dump_type fn_ret_ty <| (actual_name fn_name is_glob) <| arg_string
                let name, proc' = assign_tmp ctx codestr
                combine proc'
                {name=Some name; ty = fn_ret_ty; is_glob=false; ty_tb=types}
            | _ ->
            type_mimatch(Func([PendingTy nil_undecided], PendingTy nil_undecided), fn_ty) |> ll_raise

        | Suite suite ->
            let rec loop = function
                | [expr] ->
                    emit' ctx expr
                |  expr :: suite ->
                    emit' ctx expr |> ignore
                    loop suite
                | [] -> void_symbol
            in loop suite

        | Alloca(ty, Some(data)) ->
            let data = emit' ctx data
            if ty <> data.ty then
                Message("allocation")
                <*>
                type_mimatch(ty, data.ty)
                |> ll_raise
            else
            let ptr = {ty=Ptr ty; name = Some <| ctx.alloc_name; ty_tb=types; is_glob=false}
            let codestr = alloca' ptr <| Some data
            combine <| Ordered codestr
            ptr

        | Alloca(ty, None) ->
            let ptr = {ty=Ptr ty; name = Some <| ctx.alloc_name; ty_tb=types; is_glob=false}
            let codestr = alloca' ptr None
            combine <| Ordered codestr
            ptr

        | Load subject ->
            load_llvm <| emit' ctx subject

        | Store(subject, data) ->
            let data = emit' ctx data
            let subject = emit' ctx subject
            store_llvm subject data

        | GEP(subject, idx, offsets) ->
            let subject = emit' ctx subject
            let idx = emit' ctx idx
            match subject with
            | {ty = Ptr ty} as ptr ->
                let idx = dump_sym idx
                let ret_ty = Ptr <| find_ty types offsets ty
                let name, proc' = gep' ptr idx offsets  |> assign_tmp ctx
                combine proc'
                {name=Some name; ty = ret_ty; is_glob=false; ty_tb=types}
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`getlementptr` arg type") |> ll_raise
        | ExtractElem(subject, idx) ->
            let subject = emit' ctx subject
            let idx     = emit' ctx idx
            match subject with
            | {ty = Vec(_, ty)} ->
                let name, proc' = extractelem' subject idx |> assign_tmp ctx
                combine proc'
                {ty=ty; name = Some name; ty_tb=types; is_glob = false}
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`extractelement` arg type") |> ll_raise
        | InsertElem(subject, val', idx) ->
            let subject = emit' ctx subject
            match subject with
            | {ty = Vec(_, ty)} ->
                let val'    = emit'  ctx val'
                let idx     = emit' ctx idx
                if val'.ty =||= ty |> not then
                    Message("`insertelementptr` arg types mismatch") <*> type_mimatch(ty, val'.ty) |> ll_raise
                let name, proc' = insertelem' subject val' idx |> assign_tmp ctx
                combine proc'
                {ty = ty; name = Some name; ty_tb = types; is_glob = false}
            | {ty=ty} ->
            InvalidUsage(dump_type ty, "`insertelement` arg type") |> ll_raise


        | ExtractVal(subject, indices) ->
            let subject = emit' ctx subject
            let ty = find_ty types indices subject.ty
            let name, proc' = extractval' subject indices |> assign_tmp ctx
            combine proc'
            {ty = ty; name = Some name; ty_tb = types; is_glob=false}
        | InsertVal(subject, val', indices) ->
            let subject = emit' ctx subject
            let ty = find_ty types indices subject.ty
            let val' = emit' ctx val'
            let name, proc' = insertval' subject val' indices |> assign_tmp ctx
            combine proc'
            {ty = ty; name = Some name; ty_tb = types; is_glob=false}

        | Locate(loc, llvm) ->
            try
                emit' ctx llvm
            with LLException(exc) ->
            LocatedExc(exc, loc) |> ll_raise

    emit'
