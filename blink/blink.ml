open Riot

open Logger.Make (struct
  let namespace = [ "blink" ]
end)

module Connection = Connection
module Protocol = Protocol
module Transport = Transport
module WebSocket = Websocket
module Frame = Frame

let pp_messages = Msg.pp_messages

let connect uri =
  let ( let* ) = Result.bind in
  trace (fun f -> f "resolving %a" Uri.pp uri);
  let* addr = Net.Addr.of_uri uri in
  trace (fun f -> f "connecting %a" Net.Addr.pp addr);
  let (module Transport) = Transport.Negotiator.of_uri uri in
  trace (fun f -> f "using transport %s" Transport.name);
  let* conn = Transport.connect addr uri in
  trace (fun f -> f "connected");
  Ok conn

let request = Connection.request
let stream = Connection.stream
let messages = Connection.messages

let await conn =
  let ( let* ) = Result.bind in
  let* conn, messages = messages conn in
  let res, body = Msg.to_response messages in
  Ok (conn, res, body)
