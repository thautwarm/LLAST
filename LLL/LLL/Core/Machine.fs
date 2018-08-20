module LLL.Machine

open LLL.ASDL

type hashtable<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type context = {
    prefix                  : string
    ``global``              : hashtable<string, ``type``> // the only external scope in LLL is global scope.
    bounds                  : Map<string, ``type``>
    mutable anonymous_count : int
    }
    with 
    member this.find(name : string) = 
        match Map.tryFind name this.bounds with 
        | Some ``type`` -> ``type``
        | _         ->
        match this.``global``.TryGetValue name with 
        | (true, ``type``) -> ``type``
        | _         -> failwith <| sprintf "Name %A not found." name

type llvm = LLVM 

let mangle (prefix: string) (lexer: string) = sprintf "%s.%s" prefix lexer
    
let load (name: string) (``type``: ``type``): llvm = 
    failwith "not impl yet"

let defun (head) (body) : llvm = 
    failwith "not impl yet"

let call(func) (args) : llvm = 
    failwith "not impl yet"

let deftype (ty: ``type``) : llvm = 
    failwith "not impl yet"

let literal (data : data): llvm = 
    failwith "not impl yet"

let branch(test) (exp1) (exp2)
   
let rec emit (asdl : lll) (context: context) : llvm =
    match asdl with 
    
    | Lit data ->
        literal data
    
    | Val {actual_name = actual; lexer_name = lexer}  ->
        // LOAD
        let ``type`` = context.find lexer 
        in load actual ``type``

    | Defun {arg_tys = arg_typs
             formals = formals
             ret_ty  = ret_ty
             body    = body
             name    = name} ->
        
        let ty      =  Func(arg_typs, ret_ty)
        let head    =  load name ty
        context.``global``.[name] <- Func(arg_typs, ret_ty)
        let prefix  = mangle context.prefix name
        let context =
            // You see no closure here.
            List.zip formals arg_typs 
            |> fun val_lst -> 
            {context 
                with bounds = Map.ofList val_lst
                     prefix = prefix
                     anonymous_count = 0}

        let body = emit body context

        in defun head body
    
    | App(func, arg_lst) ->
        let load_val (value : value) = load value.actual_name <| context.find func.lexer_name
        let func = load_val func 
        let arg_lst = 
            List.map load_val arg_lst
        in call func arg_lst
    

    | Deftype(name, ``type``) -> 
        let name = mangle context.prefix name 
        context.``global``.[name] <- ``type``
        deftype(``type``)

    | Lam  {arg_tys = arg_typs
            formals = formals
            ret_ty  = ret_ty
            body    = body} ->

        let name    = sprintf "<anonymous%d>" context.anonymous_count
        let ty      =  Func(arg_typs, ret_ty)
        let head    =  load name ty
        context.``global``.[name] <- Func(arg_typs, ret_ty)  
        // TODO: 
        // if no return of the lambda occurred in function body, 
        // the type of lambda can be removed from global
        let prefix  = mangle context.prefix name
        let context =
            // You see no closure here.
            List.zip formals arg_typs 
            |> fun val_lst -> 
            {context 
                with bounds = Map.ofList val_lst
                     prefix = prefix
                     anonymous_count = 0}
        
        let body = emit body context
        in defun head body
    | IfExp(cond, exp1, exp2) ->
        
    
    



          
        
            
                

        
        



    
        
        
        
        
        
        
        





