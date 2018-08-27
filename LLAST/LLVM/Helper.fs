module LLL.LLVM.Helper
open LLL.LLVM.IR

let (|>>) (a : 'a option) (f: 'a -> 'b) : 'b =
    match a with
    | None    -> failwithf "%A is not supposed to be None." typeof<'a>
    | Some(a) -> f a
type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>

type type_table = (string,  ``type``) hashtable

type symbol = {
    name    : string option
    ty_tb   : type_table
    ty      : ``type``
    is_glob : bool
}

and context = {
    ``global`` : (string,   symbol) hashtable
    local      : (string,   symbol) hashtable
    mutable count   : int
    prefix          : string (** for context mangling usage *)
}

let fmt               = sprintf
let inline to_str arg = fmt "%s" arg
let inline concat a b = a + "." + b
let inline join lst   = String.concat ", " <| List.map to_str lst

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
        failwithf "Context %s cannot resolve symbol %s"
                 <| ctx.prefix
                 <| name

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
    | _ as it         -> failwithf "Unsupported type %A." it

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
            let ty = ty_tb.[name]
            get_align <| Set.add name recur_set <| ty
        | Alias _ -> 0
        | _  as it    -> failwithf "%A cannot be used to calculate align." it
    in
    match get_align <| set [] <| ty with
    | 0 -> 1
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
        failwith "allocation can only be applied on ptr type!"
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
    | _ -> failwithf "only pointer type could be perform `getelementptr`, got %A." ptr'.ty

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
    | PendingConst _ -> failwith "Data not decided yet."
    | ID(bit, value) -> type_data' <| I bit <| fmt "%d" value
    | FD(bit, value) -> type_data' <| F bit <| fmt "%f" value
    | ArrD lst       ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            type_data'
            <| Arr(length, ty)
            <| (fmt "[ %s ]" <| join data_lst)
        | _    -> failwith "element types mismatch"

    | VecD lst ->
        let typed_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
        match List.distinct typed_lst with
        | [ty] ->
            type_data'
            <| Vec(length, ty)
            <| (fmt "< %s >" <| join data_lst)
        | _    -> failwith "element types mismatch"
    | AggD lst ->
        let type_lst, data_lst = List.unzip <| List.map typed_data lst
        let length = List.length data_lst
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
            else failwithf "IndexError."
        | Alias(name) ->
            find_ty offsets <| types.[name]
        | _ as ty -> failwithf "Type error %A." ty 
    find_ty offsets agg_ty

let type_eq =
    function
    | Terminator, _ -> true
    | _, Terminator -> true
    | Alias name1, Alias name2 -> name1 = name2
    | a, b -> a = b

let inline (=||=) a b = type_eq(a, b)
