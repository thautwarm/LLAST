module LLL.LLVM.MLEmit
open LLL.LLVM.ML
open LLL.LLVM.Helper

type 'v arraylist = System.Collections.Generic.List<'v>

type proc =
    | Ordered  of string
    | Predef   of proc
    | Combine  of proc * proc
    | Pending  of (proc -> proc)
    | Inner    of proc
    | Empty


let pack a b = a, b


let rec emit (types: type_table) (proc: ref<proc>) =
    let rec emit' (ctx: context): (llvm -> symbol)  =
        let assign_tmp ctx codestr =
            let name = ctx.alloc_name
            name, Ordered <| fmt "%s = %s" name codestr
    
        function
        | Const constant ->
            let ty, codestr = typed_data constant
            let name, proc' = assign_tmp compiler codestr
            proc.contents <- Combine(proc.contents, proc')
            {ty = ty; ty_tb=types; actual_name=Some name; is_glob=false}
        | Get name ->
            ctx.find name

        | Let(name, value, body) ->
            let value = emit' ctx value
            ctx.local.[name] = value
            emit <| ctx.into name <| body

        | Defun(name, args, ret_ty, body) ->
            let ctx = ctx.into name
            let arg_names, arg_tys = List.unzip args
            let func_ty = Func(arg_tys, ret_ty)
            ctx.``global``.[name] <- {ty=func_ty; name=Some name; ty_tb = types; is_glob=true}
            let arg_syms =
                List.map
                <| fun (arg, ty) -> {ty = ty; name=Some <| ctx.wrap_name arg; types = types; is_glob=false}
                <| args

            let rec update_map = function
                | [] -> ()
                | (arg_name, arg_sym) :: tail -> 
                ctx.bind arg_name arg_sym
                update_map tail

            in update_map <| List.zip arg_names arg_syms
            let arg_string = List.map dump_sym arg_syms |> join
        
            let head =
                fmt "define %s %s(%s){"
                <| dump_type ret_ty
                <| name
                <| arg_string
                |> Ordered
       
            let inner_proc = ref Empty

            let body =
                emit 
                <| types
                <| inner_proc
                <| ctx
                <| body
            
            proc.contents <- Combine(Combine(proc.contents, Inner(inner_proc.Value)), fmt "}" Ordered)
            void_symbol

        | Conv conversion ->
            let routine inst sym dest =
                let codestr =
                   fmt "%s %s to %s"
                       <| inst
                       <| dump_sym sym
                       <| dump_type dest
                let compiler, name = assign_tmp compiler codestr
                compiler, {ty=dest; actual_name=name; ctx=ctx}
            
            let rec is_int_ext = function
               | I a, I b when a < b -> true
               | Vec(n, l), Vec(n', r) when n = n' -> is_int_ext(l, r)
               | _ -> false

            let compat_cast src dest =
                let {ty=ty} as sym = emit' ctx src
                let inst = 
                    let rec get_inst 

    emit'
        
