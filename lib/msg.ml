open Riot

type message =
  [ `Data of Bytestring.t
  | `Done
  | `Headers of Http.Header.t
  | `Status of Http.Status.t ]

let pp fmt (t : message) =
  match t with
  | `Headers hs -> Format.fprintf fmt "Headers(%a)" Http.Header.pp_hum hs
  | `Done -> Format.fprintf fmt "Done"
  | `Data bs -> Format.fprintf fmt "Data(%S)" (Bytestring.to_string bs)
  | `Status s -> Format.fprintf fmt "Status(%a)" Http.Status.pp s

let pp_messages fmt (t : message list) =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp fmt t
