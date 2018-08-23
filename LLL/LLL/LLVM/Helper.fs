module LLL.LLVM.Helper
open LLL.LLVM.ML

type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>

type symbol = {
    lexer_name  : string
    actual_name : string
    ctx         : context
    ty          : ``type``


}

and context = {
    ``global`` : (string, symbol) hashtable
    local      : (string, symbol) Map
    count      : int
    prefix     : string list(** for context mangling usage *)
}

let void_ctx    = {
    ``global`` = hashtable()
    local = Map.ofList []
    count=0
    prefix=[]
}
let void_symbol = {
    lexer_name = "<void_val>"
    actual_name = "<void_name>"
    ty=Void
    ctx=void_ctx
}

let fmt = sprintf


type context with
    static member alloc_name ctx =
        let count = ctx.count
        {ctx with count = count + 1}, count
    static member wrap_name ctx name =
        List.rev <| List.Cons(name, ctx.prefix) |> String.concat "."
    static member bind name ty ctx =
        let name = context.wrap_name ctx name
        let local = ctx.local
        {ctx with local = Map.add name ty local}
    static member find name ctx : symbol  =
        match Map.tryFind name ctx.local with
        | Some ty -> ty
        | None    ->
        let ref = ref void_symbol
        match ctx.``global``.TryGetValue(name, ref) with
        | true  -> ref.Value
        | false ->
        failwithf "Context %s cannot resolve symbol %s"
                 <| (List.rev >> (String.concat ".")) ctx.prefix
                 <| name

let private concat = String.concat " ,"
let to_str arg = fmt "%A" arg
let rec dump_type: ``type`` -> string =
    function
    | I bit           -> fmt "i%d" bit
    | F 32            -> "float"
    | F 64            -> "double"
    | Vec(n, ty)      -> fmt "< %d x %s >" <| n <| dump_type ty
    | Arr(n, ty)      -> fmt "[ %d x %s ]" <| n <| dump_type ty
    | Agg ty_lst      -> fmt "{ %s }" <| concat (List.map dump_type ty_lst)
    | Alias name      -> fmt "%%struct.%s" name
    | Ptr ty          -> fmt "%s *" <| dump_type ty
    | Func(args, ret) ->
        fmt "%s (%s)" <| dump_type ret <| concat (List.map dump_type args)
    | Void            -> "void"
    | _ as it         -> failwithf "Unsupported type %A." it

let inline get_align ctx ty : int =
    let rec get_align (recur_set: string Set) =
        function
        | PendingTy _ -> failwithf "Type not decided yet."
        | Void   -> 0
        | Func _ -> 0
        | F bit
        | I bit  -> bit/8
        | Ptr _  -> 8
        | Arr(_, ty)
        | Vec(_, ty)  -> 8 + get_align recur_set ty
        | Agg(ty_lst) -> List.max <| List.map (get_align recur_set) ty_lst
        | Alias name when not <| Set.contains name recur_set ->
            let ty = ctx.``global``.[name].ty
            get_align <| Set.add name recur_set <| ty
        | Alias _ -> 0
        | _  as it    -> failwithf "%A cannot be used to calculate align." it
    in
    match get_align <| set [] <| ty with
    | 0 -> 1
    | otherwise -> otherwise

let inline dump_sym {ty = ty; actual_name=actual_name} =
    let ty = dump_type ty
    fmt "%s %s" ty actual_name

let inline store' val' ptr' =
    fmt "store %s, %s" <| dump_sym val' <| dump_sym ptr'

let inline alloca' ptr' data' =
    let alloca_ty, is_ptr =
        match ptr'.ty with
        | Ptr alloca_ty -> alloca_ty, true
        | _        -> ptr'.ty, false

    if not is_ptr then
        failwith "allocation can only be applied on ptr type!"
    else
    let align = get_align ptr'.ctx alloca_ty
    match data' with
    | None ->
        fmt "alloca %s, align %d" <| dump_type alloca_ty <| align
    | Some data' ->
    fmt "alloca %s, %s ,align %d" <| dump_type alloca_ty <| dump_sym data' <| align

let inline load' {ty=ty; actual_name=ptr_name} =
    let ty = dump_type ty
    fmt "load %s, %s* %s" ty ty ptr_name

let inline gep' ptr' idx offsets =
    match ptr'.ty with
    | Ptr val_ty ->
    let val_ty = dump_type val_ty
    fmt "getelementptr inbounds %s, %s, %s, %s"
        val_ty <| dump_sym ptr' <| idx <| concat offsets
    | _ -> failwithf "only pointer type could be perform `getelementptr`, got %A." ptr'.ty

let inline extractelem' vec' idx_ty idx =
    let vec' = dump_sym vec'
    fmt "extractelement %s, %s %s" vec' idx_ty idx

let inlne insertelem' vec' val' idx' =
    fmt "insertelement %s, %s, %s"
    <| dump_sym vec'
    <| dump_sym val'
    <| dump_sym idx'

let inline extractval' agg' (offsets: int list) =
    fmt "extractvalue %s %s, %s"
    <| dump_sym agg'
    <| concat (List.map to_str offsets)

let inline insertval' agg' elt' offsets =
    fmt "insertvalue %s, %s, %s"
        <| dump_sym agg'
        <| dump_sym elt'
        <| concat (List.map to_str offsets)

let rec typed_data: constant -> (``type`` * string) = function
    | PendingConst _ -> failwith "Data not decided yet."
    | ID(bit, value) -> I bit, to_str value
    | FD(bit, value) -> F bit, to_str value
    | ArrD lst       ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            Arr(length, ty),
            fmt "[ %s ]" <| concat data_lst
        | _    -> failwith "element types mismatch"
    | VecD lst ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            Vec(length, ty),
            fmt "< %s >" <| concat data_lst
        | _    -> failwith "element types mismatch"
    | AggD lst ->
        let type_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        let agg_ty = Agg(type_lst)
        agg_ty,
        fmt "{ %s }" <| concat data_lst
    | Undef ty ->
        ty, "undef"



let type_eq =
    function
    | Wildcard, _ -> true
    | _, Wildcard -> true
    | Alias name1, Alias name2 -> name1 = name2
    | a, b -> a = b
