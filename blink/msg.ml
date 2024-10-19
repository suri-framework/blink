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
  | `Data bs -> Format.fprintf fmt "Data(%d bytes)" (Bytestring.length bs)
  | `Status s -> Format.fprintf fmt "Status(%a)" Http.Status.pp s

let pp_messages fmt (t : message list) =
  Format.fprintf fmt "[\n  ";
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n  ")
    pp fmt t;
  Format.fprintf fmt "\n]"

let to_response messages =
  let status, headers, body =
    messages
    |> List.fold_left
         (fun ((status, headers, body) as acc) msg ->
           match msg with
           | `Status s -> (Some s, headers, body)
           | `Headers hs -> (status, Some hs, body)
           | `Data data -> (status, headers, Bytestring.(body ^ data))
           | `Done -> acc)
         (None, None, Bytestring.empty)
  in
  (Http.Response.make ?status ?headers (), body)
