module Machine

open LLL.ASDL

type hashmap<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type context = {
    prefix      : string
    ``global``  : hashmap<string, ``type``> // the only external scope in LLL is global scope.
    bounds      : Map<string, ``type``>
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

let define (context: context) 
           (name: string) (ret: ``type``) (take: ``type``) (body: lll) =
   
let rec emit (asdl : lll) (context: context) =
    match asdl with 
    | Lit data  -> 
        failwith "not impl yet"
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
            List.fold 
            <| fun context (formal: string) -> 
               let name = mangle context.prefix formal
               let value = {actual_name = name; lexer_name = formal}
               {context with bounds = context.bounds.Add (formal, value)}
           <| context <| formals
        let body = emit body context 
        in 
            
            
                

        
        



    
        
        
        
        
        
        
        





