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
  error (fun f -> f "resolving %a" Uri.pp uri);
  let* addr = Net.Addr.of_uri uri in
  error (fun f -> f "connecting %a" Net.Addr.pp addr);
  let (module Transport) = Transport.Negotiator.of_uri uri in
  error (fun f -> f "using transport %s" Transport.name);
  let* conn = Transport.connect addr uri in
  error (fun f -> f "connected");
  Ok conn

let request = Connection.request
let stream = Connection.stream
