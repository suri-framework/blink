open Riot

module type Intf = sig
  val connect :
    Net.Addr.stream_addr ->
    Uri.t ->
    ( Connection.t,
      [> `Closed | `Unix_error of Unix.error | `Tls_error of exn ] )
    result
end

module Tcp : Intf = Tcp
module Ssl : Intf = Ssl

module Negotiator = struct
  let of_uri uri =
    match Uri.scheme uri |> Option.map String.lowercase_ascii with
    | Some "https" -> (module Ssl : Intf)
    | Some "http" | _ -> (module Tcp : Intf)
end
