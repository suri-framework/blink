open Riot

open Logger.Make (struct
  let namespace = [ "blink"; "connection" ]
end)

let ( let* ) = Result.bind

type message = Msg.message

type state =
  | Done
  | Unread
  | More of { body_remaining : int; prefix : Bytestring.t }

type t =
  | Conn : {
      protocol : (module Protocol.Intf);
      writer : 'socket IO.Writer.t;
      reader : 'socket IO.Reader.t;
      uri : Uri.t;
      addr : Net.Addr.stream_addr;
      headers : Http.Header.t;
      status : Http.Status.t;
      state : state;
    }
      -> t

let make ~reader ~writer ~uri ~addr =
  Conn
    {
      writer;
      reader;
      uri;
      addr;
      protocol = (module Protocol.Http1);
      status = `OK;
      headers = Http.Header.of_list [];
      state = Unread;
    }

let send (Conn { writer; _ } as conn) data =
  error (fun f ->
      let bufs = Bytestring.to_iovec data in
      f "sending %d octets (iovec)" (IO.Iovec.length bufs));
  let buf = Bytestring.to_string data in
  let* () = IO.write_all writer ~buf in
  Ok conn

let receive (Conn { reader; _ } as conn) =
  let* data = Bytestring.with_bytes (fun buf -> IO.read reader buf) in
  Ok (conn, data)

let request (Conn conn) req ?body () =
  let (module Protocol : Protocol.Intf) = conn.protocol in
  let buf = Protocol.Request.to_buffer conn.uri req ?body () in
  let* () = IO.write_all conn.writer ~buf:(Bytestring.to_string buf) in
  Ok (Conn conn)

let stream (Conn conn) =
  let (module Protocol : Protocol.Intf) = conn.protocol in
  match conn.state with
  | Unread ->
      let* status, headers, prefix =
        Protocol.Response.read_header conn.reader
      in
      let body_remaining =
        let content_length =
          Http.Header.get_content_range headers
          |> Option.value ~default:0L |> Int64.to_int
        in
        debug (fun f ->
            f "just read prefix=%d out of %d" (Bytestring.length prefix)
              content_length);
        if content_length > 0 then content_length - Bytestring.length prefix
        else 0
      in

      let parts = [ `Status status; `Headers headers ] in

      Ok
        ( Conn
            {
              conn with
              status;
              headers;
              state = More { prefix; body_remaining };
            },
          parts )
  | More { body_remaining; prefix } -> (
      debug (fun f ->
          f "streaming more body_remaining=%d prefix=%a" body_remaining
            Bytestring.pp prefix);
      match
        Protocol.Response.read_body ~prefix ~headers:conn.headers
          ~body_remaining conn.reader
      with
      | `Error reason -> Error reason
      | `Ok [] -> Ok (Conn { conn with state = Done }, [ `Done ])
      | `Ok parts ->
          let parts = List.map (fun p -> `Data p) parts in
          Ok (Conn { conn with state = Done }, parts)
      | `More (parts, prefix, body_remaining) ->
          let parts = List.map (fun p -> `Data p) parts in
          Ok (Conn { conn with state = More { body_remaining; prefix } }, parts)
      )
  | Done -> Ok (Conn conn, [ `Done ])

let messages conn =
  let rec consume_stream conn messages =
    let* conn, msgs = stream conn in
    match msgs with
    | [ `Done ] -> Ok (List.rev messages)
    | _ -> consume_stream conn (msgs @ messages)
  in
  consume_stream conn []
