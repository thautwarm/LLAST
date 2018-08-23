module LLL.LLVM.Helper
open LLL.LLVM.ML
type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>
let fmt = sprintf
type context = {
    ``global`` : (string, ``type``) hashtable
    local      : (string, ``type``) Map
    count      : int
    prefix     : string list(** for context mangling usage *)
}

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
    static member find name ctx : ``type`` =
        match Map.tryFind name ctx.local with
        | Some ty -> ty
        | None    ->
        let ref = ref Void
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
    | I bit -> fmt "i%d" bit
    | F 32  -> "float"
    | F 64  -> "double"
    | Vec(n, ty) -> fmt "< %d x %s >" <| n <| dump_type ty
    | Arr(n, ty) -> fmt "[ %d x %s ]" <| n <| dump_type ty
    | Agg ty_lst -> fmt "{ %s }" <| concat (List.map dump_type ty_lst)
    | Named name -> fmt "%%struct.%s" name
    | Ptr ty     -> fmt "%s *" <| dump_type ty
    | Func(args, ret) ->
        fmt "%s (%s)" <| dump_type ret <| concat (List.map dump_type args)
    | Void       -> "void"
    | _ as it    -> failwithf "Unsupported type %A." it

let inline get_align ty : int =
    let rec get_align =
        function
        | Void   -> 0
        | Func _ -> 0
        | F bit
        | I bit  -> bit/8
        | Ptr _  -> 8
        | Arr(_, ty)
        | Vec(_, ty)  -> 8 + get_align ty
        | Agg(ty_lst) -> List.max <| List.map get_align ty_lst
        | Named name  -> failwithf "get align mustn't accept a named type."
    in
    match get_align ty with
    | 0 -> 1
    | otherwise -> otherwise

let inline store' ty val_name ptr_name =
    let ty = dump_type ty
    fmt "store %s %s, %s %s" ty val_name ptr_name

let inline alloca' ty maybe_val_name =
    let align = get_align ty
    let ty    = dump_type ty
    match maybe_val_name with
    | None -> fmt "alloca %s, align %d" ty align
    | Some val_name -> fmt "alloca %s, %s %s, align %d" ty ty val_name align
let inline load' ty ptr_name =
    let ty = dump_type ty
    fmt "load %s, %s* %s" ty ty ptr_name

let inline gep' ty ptr_name idx offsets =
    let ty = dump_type ty
    fmt "getelementptr inbounds %s, %s* %s, %s, %s"
        ty ty ptr_name idx <| concat offsets

let inline extractelem' vec_ty vec_name idx_ty idx =
    let vec_ty = dump_type vec_ty
    let idx_ty = dump_type idx_ty
    fmt "extractelement %s %s, %s %s" vec_ty vec_name idx_ty idx

let inlne insertelem' vec_ty vec_name val_ty val_name idx_ty idx =
    let vec_ty = dump_type vec_ty
    let idx_ty = dump_type idx_ty
    let val_ty = dump_type val_ty
    fmt "insertelement %s %s, %s %s, %s %s" vec_ty vec_name val_ty val_name idx_ty idx

let inline extractval' agg_ty agg_name (offsets: int list) =
    let agg_ty = dump_type agg_ty
    fmt "extractvalue %s %s, %s" agg_ty agg_name <| concat (List.map to_str offsets)

let inline insertval' agg_ty agg_name elt_ty elt_name offsets =
    let agg_ty = dump_type agg_ty
    let elt_ty = dump_type elt_ty
    fmt "insertvalue %s %s, %s %s, %s"
        agg_ty agg_name
        elt_ty elt_name
        <| concat (List.map to_str offsets)

let typed_data: constant -> (``type`` * string) = function
    | ID(bit, value) -> I bit, to_str value
    | 
