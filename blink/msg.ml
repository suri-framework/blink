open Riot

type message =
  [ `Data of Bytestring.t
  | `Done
  | `Headers of Http.Header.t
  | `Status of Http.Status.t ]

let pp fmt (t : message) =
  match t with
  | `Headers _ -> Format.fprintf fmt "Headers"
  | `Done -> Format.fprintf fmt "Done"
  | `Data bs -> Format.fprintf fmt "Data %a" Bytestring.pp bs
  | `Status _ -> Format.fprintf fmt "Status"

let pp_messages fmt (t : message list) =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp fmt t
