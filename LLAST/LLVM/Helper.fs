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
            consts     = hashtable()
            count      = 0
            prefix     = "ll"
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
    | U bit
    | I bit           -> fmt "i%d" bit
    | F 32            -> "float"
    | F 64            -> "double"
    | Vec(n, ty)      -> fmt "< %d x %s >" <| n <| dump_type ty
    | Arr(n, ty)      -> fmt "[ %d x %s ]" <| n <| dump_type ty
    | Agg ty_lst      -> fmt "{ %s }" <| join (List.map dump_type ty_lst)
    | Alias name      -> fmt "%%.struct.%s" name
    | Ptr ty          -> fmt "%s*" <| dump_type ty
    | Func(args, ret) ->
        fmt "%s (%s)" <| dump_type ret <| join (List.map dump_type args)
    | Void            -> "void"
    | Label           -> "label"
    | _ as it         -> failwithf "Type %A cannot be dumped." it


let type_mimatch(expected: ``type``, got: ``type``) = UnexpectedUsage(dump_type got, dump_type expected, "type equation constriant")

let type_substitute (sub_map: (string, string) hashtable) ty =
    let rec sub ty =
        match ty with
        | Label
        | Terminator
        | PendingTy _
        | U _
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
        | U bit
        | F bit
        | I bit  -> bit/8
        | Ptr _  -> 8
        | Arr(_, ty)
        | Vec(_, ty)  -> get_align recur_set ty
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

let inline get_size_and_align(ty_tb: type_table) ty : int64 * int64 =
    let rec get_size (recur_set: string Set) = 
        function
        | PendingTy id -> NotDecidedYet id |> ll_raise
        | Void         -> InvalidUsage("type void", "sizeof") |> ll_raise
        | Func _       -> InvalidUsage("type function", "sizeof") |> ll_raise
        | U bit
        | I bit
        | F bit        -> bit/8
        | Ptr _        -> 8
        | Arr(n, ty)
        | Vec(n, ty)   -> get_size recur_set ty |> (*) n
        | Agg(ty_lst)  -> List.sum <| List.map (get_size recur_set) ty_lst
        | Alias name when not <| Set.contains name recur_set ->
            let ty = ty_tb.[name]
            get_size <| Set.add name recur_set <| ty
        | Alias name     -> InvalidUsage(fmt "value type of %s" name, "recursive type") |> ll_raise
        | _  as it    -> InvalidUsage(dump_type it, "get_align") |> ll_raise
    in
    let align = get_align ty_tb ty
    let total = get_size (set[]) <| ty |> decimal 
    in  
    let chunk_num = System.Math.Ceiling(total / decimal align) |> System.Convert.ToInt64
    let align = System.Convert.ToInt64 align
    chunk_num * align, align

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

let inline load' sym =
    match sym.ty with
    | Ptr ty ->
        let ty = dump_type ty
        fmt "load %s, %s* %s" ty ty <| actual_name sym.name sym.is_glob
    | _      -> UnexpectedUsage(dump_type sym.ty, "Pointer type", "`load`") |> ll_raise

let inline gep' ptr' idx offsets =
    match ptr'.ty with
    | Ptr val_ty ->
    let val_ty = dump_type val_ty
    if List.isEmpty offsets then 
        fmt "getelementptr inbounds %s, %s, %s" <| val_ty <| dump_sym ptr' <| idx
    else fmt "getelementptr inbounds %s, %s, %s, %s"
              val_ty <| dump_sym ptr' <| idx <| join (List.map (fmt "i32 %d") offsets)
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
    
let private (%%) ty str =
    ty, fun ctx -> fmt "%s %s" <| dump_type ty <| str

let private (%>) ty fn =
    ty, fun ctx -> fmt "%s %s" <| dump_type ty <| fn(ctx)

let rec typed_data: constant -> (``type`` * (context -> string)) = 
    function
    | PendingConst id -> NotDecidedYet(id) |> ll_raise
    | UD(bit, value)  -> U bit %% fmt "%u" value
    | ID(bit, value)  -> I bit %% fmt "%d" value
    | FD(bit, value)  -> F bit %% fmt "%f" value
    | ArrD lst        ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | []   -> failwith "Impossible"
        | [ty] ->
            Arr(length, ty)
            %>
            fun ctx -> 
                let eval = fun it -> it(ctx)
                (fmt "[ %s ]" <| join (List.map eval data_lst))
        | a :: b :: _ -> type_mimatch(a, b) |> ll_raise

    | VecD lst ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | []   -> failwith "Impossible"
        | [ty] ->
            Vec(length, ty)
            %>
            fun ctx -> 
                let eval = fun it -> it(ctx)
                (fmt "< %s >" <| join (List.map eval data_lst))
        | a :: b :: _ -> type_mimatch(a, b) |> ll_raise

    | AggD lst ->
        let type_lst, data_lst = List.unzip <| List.map typed_data lst
        let agg_ty = Agg(type_lst)
        agg_ty
        %>
        fun ctx -> 
            let eval = fun it -> it(ctx)
            (fmt "{ %s }" <| join (List.map eval data_lst))

    | BlockAddr(fn_name, label_name) ->
        (Ptr <| I 8)
        %>
        fun ctx ->
            let fn = ctx.find fn_name
            let label: symbol = ctx.find label_name
            let fn = actual_name fn.name fn.is_glob
            let label = actual_name label.name label.is_glob
            fmt "blockaddress(%s, %s)" fn label

    | Undef ty ->
        ty %% "undef"


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
            UnexpectedUsage(fmt "index %d" offset, 
                            fmt "index n, n < %d" n, 
                            fmt "Index on type %s" 
                            <| dump_type agg_ty) 
            |> ll_raise
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

let get_constant (types: type_table) (ctx: context) (constant: constant) = 
    let ref = ref <| void_symbol
    match ctx.consts.TryGetValue(constant, ref) with
    | true  -> ref.Value, Empty
    | false ->
    let ty, code_str_getter = typed_data constant
    let name = fmt ".const.%d" ctx.consts.Count
    let sym = {ty=Ptr ty; name = Some name; is_glob = true; ty_tb = types}
    ctx.consts.[constant] <- sym

    let pending_code() =
        fmt "@%s = private unnamed_addr constant %s, align %d" name 
        <| code_str_getter(ctx) 
        <| get_align types ty 
        |> Ordered
    sym,
    Pending <| pending_code |> Predef
        
let inline (=||=) a b = type_eq(a, b)
