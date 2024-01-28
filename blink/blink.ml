open Riot
module Connection = Connection
module Protocol = Protocol
module Transport = Transport
module WebSocket = Websocket

let pp_messages = Msg.pp_messages

let connect uri =
  let ( let* ) = Result.bind in
  let* addr = Net.Addr.of_uri uri in
  let (module Transport) = Transport.Negotiator.of_uri uri in
  let* conn = Transport.connect addr uri in
  Connection.upgrade conn

let request = Connection.request
let stream = Connection.stream
