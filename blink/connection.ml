open Riot

let ( let* ) = Result.bind

type message = Msg.message

type t =
  | Conn : {
      protocol : (module Protocol.Intf);
      writer : 'socket IO.Writer.t;
      reader : 'socket IO.Reader.t;
      uri : Uri.t;
      addr : Net.Addr.stream_addr;
    }
      -> t

let make ~reader ~writer ~uri ~addr =
  Conn { writer; reader; uri; addr; protocol = (module Protocol.Http1) }

let upgrade (Conn conn) = Ok (Conn conn)

let request (Conn conn) req ?body () =
  let (module Protocol : Protocol.Intf) = conn.protocol in
  let data = Protocol.Request.to_buffer conn.uri req ?body () in
  let* _bytes = IO.Writer.write ~data conn.writer in
  Ok (Conn conn)

let stream (Conn conn) =
  let (module Protocol : Protocol.Intf) = conn.protocol in
  let buf = IO.Buffer.with_capacity (1024 * 2) in
  let* parts = Protocol.Response.of_reader conn.reader ~buf in
  Ok (Conn conn, parts)
