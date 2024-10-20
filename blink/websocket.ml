open Riot

open Logger.Make (struct
  let namespace = [ "blink"; "websocket" ]
end)

let ( let* ) = Result.bind

type t = { conn : Connection.t; buffer : Bytestring.t }

let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let validate_nonce request_nonce headers =
  let* response_nonce =
    Http.Header.get headers "sec-websocket-accept"
    |> Option.to_result ~none:(`Msg "sec-websocket-accept was not present")
  in

  let expected_nonce =
    Digestif.SHA1.(
      digest_string (request_nonce ^ websocket_guid) |> to_raw_string)
    |> Base64.encode_string
  in

  if String.equal response_nonce expected_nonce then Ok ()
  else Error (`Msg "invalid response nonce")

let upgrade conn path =
  let nonce = Crypto.Random.string 16 |> Base64.encode_string in

  (* let extension = [] in *)
  let req =
    let headers =
      Http.Header.of_list
        [
          ("Sec-WebSocket-Version", "13");
          ("Sec-WebSocket-Key", nonce);
          ("Sec-WebSocket-Extensions", "");
          ("Connection", "Upgrade");
          ("Upgrade", "websocket");
        ]
    in
    Http.Request.make ~headers ~meth:`GET path
  in
  let* conn = Connection.request conn req () in
  let* conn, messages = Connection.messages conn in

  match messages with
  | [ `Headers headers; `Status `Switching_protocols; `Data _ ] ->
      let* () = validate_nonce nonce headers in
      Ok { conn; buffer = Bytestring.empty }
  | _ -> Error (`Msg "unexpected response when upgrading")

let send frames t =
  List.iter
    (fun frame -> trace (fun f -> f "sending frames: %a" Frame.pp frame))
    frames;
  let data =
    Bytestring.concat Bytestring.empty (List.map Frame.Request.serialize frames)
  in
  let* conn = Connection.send t.conn data in
  Ok { t with conn }

let receive { conn; buffer } =
  let* conn, data = Connection.receive conn in

  let data =
    if Bytestring.is_empty buffer then data else Bytestring.(buffer ^ data)
  in

  debug (fun f -> f "unfolding frames from: %S" (Bytestring.to_string data));
  let* frames, buffer =
    Stream.unfold Frame.Response.deserialize (Bytestring.to_string data)
    |> Stream.reduce_while (Ok ([], Bytestring.empty)) @@ fun frame state ->
       match (frame, state) with
       | `ok frame, Ok (acc, buff) -> `continue (Ok (frame :: acc, buff))
       | `more buff, Ok (acc, _) -> `halt (Ok (acc, buff))
       | `error reason, _ -> `halt (Error reason)
       | _, Error reason -> `halt (Error reason)
  in

  Ok ({ conn; buffer }, frames)
