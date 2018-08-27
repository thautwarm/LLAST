module LLVM.Helper
open LLVM.IR
open LLVM.Infras
open LLVM.Exc

let (|>>) (a : 'a option) (f: 'a -> 'b) : 'b =
    match a with
    | None    -> NullExc <| typeof<'a> |> ll_raise
    | Some(a) -> f a


let void_symbol = {
    name        = None
    ty          = Void
    ty_tb       = hashtable()
    is_glob     = true
}

let terminator = {
    name        = None
    ty          = Terminator
    ty_tb       = hashtable()
    is_glob     = true
}

let nil_undecided: ``type`` undecided = {
    id = -1;
}

type context with
    static member init =
        {
            ``global`` = hashtable()
            local      = hashtable()
            count      = 0
            prefix     = ""
        }
    member ctx.alloc_name =
        let count = ctx.count
        ctx.count <- count + 1
        ctx.wrap_name <| fmt "%d" count

    member this.into name =
        {this with count = 0; prefix = concat this.prefix name; local = hashtable(this.local)}

    member ctx.wrap_name name : string =
        concat ctx.prefix name

    member ctx.bind name sym =
        let local   = ctx.local
        local.[name] <- sym

    member ctx.find name : symbol  =
        let ref = ref void_symbol
        match ctx.local.TryGetValue(name, ref)with
        | true  -> ref.Value
        | false ->
        match ctx.``global``.TryGetValue(name, ref) with
        | true  -> ref.Value
        | false ->
        NameNotFound(name, ctx) |> ll_raise
let rec dump_type: ``type`` -> string =
    function
    | I bit           -> fmt "i%d" bit
    | F 32            -> "float"
    | F 64            -> "double"
    | Vec(n, ty)      -> fmt "< %d x %s >" <| n <| dump_type ty
    | Arr(n, ty)      -> fmt "[ %d x %s ]" <| n <| dump_type ty
    | Agg ty_lst      -> fmt "{ %s }" <| join (List.map dump_type ty_lst)
    | Alias name      -> fmt "%%.struct.%s" name
    | Ptr ty          -> fmt "%s *" <| dump_type ty
    | Func(args, ret) ->
        fmt "%s (%s)" <| dump_type ret <| join (List.map dump_type args)
    | Void            -> "void"
    | _ as it         -> failwithf "Type %A cannot be dumped." it


let type_mimatch(expected: ``type``, got: ``type``) = UnexpectedUsage(dump_type got, dump_type expected, "type equation constriant")

let type_substitute (sub_map: (string, string) hashtable) ty =
    let rec sub ty =
        match ty with
        | Label
        | Terminator
        | PendingTy _
        | I _
        | F _             -> ty
        | Vec(n, ty)      -> Vec(n, sub ty)
        | Arr(n, ty)      -> Arr(n, sub ty)
        | Agg ty_lst      -> Agg(List.map sub ty_lst)
        | Ptr ty          -> Ptr <| sub ty
        | Func(args, ret) -> Func(List.map sub args, sub ret)
        | Void            -> Void
        | Alias name      ->
        let ty' = ref ""
        match sub_map.TryGetValue(name, ty') with
        | true  -> Alias(ty'.Value)
        | false -> ty

    in sub ty

let inline get_align (ty_tb: type_table) ty : int =
    let rec get_align (recur_set: string Set) =
        function
        | PendingTy id -> NotDecidedYet id |> ll_raise
        | Void   -> 0
        | Func _ -> 0
        | F bit
        | I bit  -> bit/8
        | Ptr _  -> 8
        | Arr(_, ty)
        | Vec(_, ty)  -> 8 + get_align recur_set ty
        | Agg(ty_lst) -> List.max <| List.map (get_align recur_set) ty_lst
        | Alias name when not <| Set.contains name recur_set ->
            let ty = ty_tb.[name]
            get_align <| Set.add name recur_set <| ty
        | Alias _     -> 0
        | _  as it    -> InvalidUsage(dump_type it, "get_align") |> ll_raise
    in
    match get_align <| set [] <| ty with
    | 0         -> 1
    | otherwise -> otherwise

let inline actual_name name is_glob =
    name |>> fun name -> name |> if is_glob then fmt "@%s" else fmt "%%%s"

let inline dump_sym {ty = ty; name=name; is_glob=is_glob} =
    let ty = dump_type ty
    fmt "%s %s" ty <| actual_name name is_glob

let inline store' val' ptr' =
    fmt "store %s, %s" <| dump_sym val' <| dump_sym ptr'

let inline alloca' ptr' data' =
    let alloca_ty, is_ptr =
        match ptr'.ty with
        | Ptr alloca_ty -> alloca_ty, true
        | _             -> ptr'.ty, false

    if not is_ptr then
        InvalidUsage(dump_type ptr'.ty, "allocation") |> ll_raise
    else
    let align = get_align ptr'.ty_tb alloca_ty
    match data' with
    | None ->
        fmt "%s = alloca %s, align %d" <| actual_name ptr'.name false <| dump_type alloca_ty <| align
    | Some data' ->
    fmt "%s = alloca %s, %s ,align %d" <| actual_name ptr'.name false <| dump_type alloca_ty <| dump_sym data' <| align

let inline load' {ty=ty; name=ptr_name} =
    ptr_name |>>
    fun ptr_name ->
    let ty = dump_type ty
    fmt "load %s, %s* %s" ty ty ptr_name

let inline gep' ptr' idx offsets =
    match ptr'.ty with
    | Ptr val_ty ->
    let val_ty = dump_type val_ty
    fmt "getelementptr inbounds %s, %s, %s, %s"
        val_ty <| dump_sym ptr' <| idx <| join (List.map (fmt "%d") offsets)
    | _ as ty -> UnexpectedUsage(dump_type ty, "Pointer type", "`getlementptr`") |> ll_raise

let inline extractelem' vec' idx': string =
    let vec' = dump_sym vec'
    let idx  = dump_sym idx'
    fmt "extractelement %s, %s" vec' idx

let inline insertelem' vec' val' idx' =
    fmt "insertelement %s, %s, %s"
    <| dump_sym vec'
    <| dump_sym val'
    <| dump_sym idx'

let inline extractval' agg' (offsets: int list): string =
    fmt "extractvalue %s, %s"
    <| dump_sym agg'
    <| join (List.map (fmt "%d") offsets)

let inline insertval' agg' elt' offsets =
    fmt "insertvalue %s, %s, %s"
        <| dump_sym agg'
        <| dump_sym elt'
        <| join (List.map (fmt "%d") offsets)

let private type_data' ty str =
    ty, fmt "%s %s" <| dump_type ty <| str
let rec typed_data: constant -> (``type`` * string) = function
    | PendingConst id -> NotDecidedYet(id) |> ll_raise
    | ID(bit, value)  -> type_data' <| I bit <| fmt "%d" value
    | FD(bit, value)  -> type_data' <| F bit <| fmt "%f" value
    | ArrD lst        ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | []   -> failwith "Impossible"
        | [ty] ->
            type_data'
            <| Arr(length, ty)
            <| (fmt "[ %s ]" <| join data_lst)
        | a :: b :: _ -> type_mimatch(a, b) |> ll_raise

    | VecD lst ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | []   -> failwith "Impossible"
        | [ty] ->
            type_data'
            <| Vec(length, ty)
            <| (fmt "< %s >" <| join data_lst)
        | a :: b :: _ -> type_mimatch(a, b) |> ll_raise

    | AggD lst ->
        let type_lst, data_lst = List.unzip <| List.map typed_data lst
        let agg_ty = Agg(type_lst)
        type_data'
        <| agg_ty
        <| (fmt "{ %s }" <| join data_lst)
    | Undef ty ->
        type_data' ty "undef"

let rec find_ty (types: type_table) offsets agg_ty =
    let rec find_ty offsets agg_ty =
        match offsets with
        | [] -> agg_ty
        | offset :: offsets ->
        match agg_ty with
        | Agg(list) -> find_ty offsets list.[offset]
        | Vec(n, ty) ->
            if n < offset then find_ty offsets ty
            else
            UnexpectedUsage(fmt "index %d" offset, fmt "index n, n < %d" n, fmt "Index on type %s" <| dump_type agg_ty) |> ll_raise
        | Alias(name) ->
            find_ty (offset :: offsets) <| types.[name]
        | _ as ty -> InvalidUsage("Get member", dump_type ty) |> ll_raise
    find_ty offsets agg_ty

let type_eq =
    function
    | Terminator, _ -> true
    | _, Terminator -> true
    | Alias name1, Alias name2 -> name1 = name2
    | a, b -> a = b

let inline (=||=) a b = type_eq(a, b)
