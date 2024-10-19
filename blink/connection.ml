open Riot

open Logger.Make (struct
  let namespace = [ "blink"; "connection" ]
end)

let ( let* ) = Result.bind

let pp_err fmt (err : [ IO.io_error | `Excess_body_read ]) =
  match err with
  | `Excess_body_read -> Format.fprintf fmt "Excess_body_read"
  | #IO.io_error as err -> IO.pp_err fmt err

type message = Msg.message
type measure = [ `unknown | `fixed of int ]

let pp_measure fmt m =
  match m with
  | `unknown -> Format.fprintf fmt "unknown"
  | `fixed size -> Format.fprintf fmt "%d" size

let measure_to_int m = match m with `unknown -> 0 | `fixed size -> size

type state =
  | Done
  | Unread
  | More of { body_remaining : measure; body_prefix : Bytestring.t }

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
  trace (fun f ->
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

let stream (Conn ({ headers; reader; state; protocol; _ } as conn)) =
  let (module Protocol : Protocol.Intf) = protocol in
  match state with
  | Done -> Ok (Conn conn, [ `Done ])
  (*
      NOTE(@leostera): when we start streaming on a connection, the first things
      we want to get out of it are going to be the status and the header. This will
      give us more information about how to read the body, if there's a body at all.

      However, sometimes when we read the first chunks of data sent to us, we also
      find a portion of the body itself. We call this the `body_prefix`.

      At this point we can emit the `` `Status `` and `` `Header `` messages,
      and if the remaining body (`body_remaining`) is not zero, then we save the
      `body_prefix` for the next call of `stream conn`.

      When the `body_remaining` is zero, then we have read the entire length of
      the body, and we can return the `` `Data `` and `` `Done `` messages as
      well.
  *)
  | Unread ->
      debug (fun f -> f "Beginning stream on unread connection...");
      let* status, headers, body_prefix =
        Protocol.Response.read_header reader
      in
      debug (fun f -> f "-> status: %a" Http.Status.pp status);
      debug (fun f ->
          f "-> body prefix size: %d" (Bytestring.length body_prefix));
      let content_length =
        match Http.Header.get_content_range headers with
        | None -> `unknown
        | Some size -> `fixed (Int64.to_int size)
      in
      debug (fun f ->
          f "-> expected content length: %a" pp_measure content_length);
      let body_remaining =
        match content_length with
        | `unknown | `fixed 0 -> content_length
        | `fixed size -> `fixed (size - Bytestring.length body_prefix)
      in
      debug (fun f -> f "-> remaining body: %a" pp_measure body_remaining);

      let messages = [ `Status status; `Headers headers ] in

      let state, messages =
        match body_remaining with
        (* when we know we've read the entire body, we finish the streaming *)
        | `fixed 0 -> (Done, messages @ [ `Data body_prefix; `Done ])
        (* we the length is unknown, or there is more body to read, we continue streaming *)
        | `fixed _ | `unknown -> (More { body_remaining; body_prefix }, messages)
      in

      Ok (Conn { conn with status; headers; state }, messages)
  (*
      NOTE(@leostera): when we poll on the connection to read more of the body,
      we either know exactly how much body is remaining, or we're it is unknown
      (for streaming large bodies this is common).

      If the remaining is zero, we mark the stream as completed, and return the
      `` `Done `` message.

      If the remaining is larger than zero, or it is unknown, we will attempt
      to read more of the body.

      The result from reading can be either an error, an indication that we
      have reached the end of the stream, or that we should continue reading.
  *)
  | More { body_remaining = `fixed 0; _ } ->
      Ok (Conn { conn with state = Done }, [ `Done ])
  | More { body_remaining; body_prefix } -> (
      debug (fun f -> f "Continuing stream on connection...");
      debug (fun f -> f "-> body_remaining: %a" pp_measure body_remaining);
      match
        let body_remaining = measure_to_int body_remaining in
        Protocol.Response.read_body ~buffer:body_prefix ~headers ~body_remaining
          reader
      with
      | `error reason ->
          error (fun f -> f "stream error: %a" pp_err reason);
          Error reason
      | `finished part ->
          let msgs =
            if Bytestring.length part = 0 then [ `Done ]
            else [ `Data part; `Done ]
          in
          Ok (Conn { conn with state = Done }, msgs)
      | `continue (body, body_prefix) ->
          let body_remaining =
            match body_remaining with
            | `unknown -> `unknown
            | `fixed total ->
                `fixed Bytestring.(total - length body - length body_prefix)
          in
          Ok
            ( Conn { conn with state = More { body_remaining; body_prefix } },
              [ `Data body ] ))

let messages conn =
  let rec consume_stream conn messages =
    let* conn, msgs = stream conn in
    match msgs with
    | [] | [ `Done ] | `Done :: _ -> Ok (conn, List.rev msgs @ messages)
    | _ -> consume_stream conn (List.rev msgs @ messages)
  in
  let* conn, messages = consume_stream conn [] in
  let messages = List.rev messages in
  debug (fun f ->
      f "collected %d messages:\n%a" (List.length messages) Msg.pp_messages
        messages);
  Ok (conn, messages)
