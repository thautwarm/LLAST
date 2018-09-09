module LL.Infras
let fmt               = sprintf
let inline to_str arg = fmt "%s" arg
let inline concat a b = a + "." + b
let inline join lst   = String.concat ", " <| List.map to_str lst

type ('k, 'v) hashtable = System.Collections.Generic.Dictionary<'k, 'v>

