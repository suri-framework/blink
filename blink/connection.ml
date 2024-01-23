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
  let buf = Protocol.Request.to_buffer conn.uri req ?body () in
  let* () =
    IO.write_all conn.writer
      ~buf:(Bytes.unsafe_of_string (Bytestring.to_string buf))
  in
  Ok (Conn conn)

let stream (Conn conn) =
  let (module Protocol : Protocol.Intf) = conn.protocol in
  let* parts = Protocol.Response.of_reader conn.reader in
  Ok (Conn conn, parts)
