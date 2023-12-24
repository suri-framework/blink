open Riot
module Connection = Connection
module Protocol = Protocol
module Transport = Transport

let ( let* ) = Result.bind

let connect uri =
  let* addr =
    Net.Addr.of_uri uri |> Option.to_result ~none:(`Invalid_uri uri)
  in
  let (module Transport) = Transport.Negotiator.of_uri uri in
  let* conn = Transport.connect addr uri in
  Connection.upgrade conn

let request = Connection.request
let stream = Connection.stream
