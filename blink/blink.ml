open Riot

let ( let* ) = Result.bind

module X509 = struct
  let read_file path =
    let ic = open_in path in
    let data = In_channel.input_all ic in
    Cstruct.of_string data

  let extension str =
    let n = String.length str in
    let rec scan = function
      | i when i = 0 -> None
      | i when str.[i - 1] = '.' -> Some (String.sub str i (n - i))
      | i -> scan (pred i)
    in
    scan n

  let certs_of_cstruct pem =
    match X509.Certificate.decode_pem_multiple pem with
    | Ok cs -> cs
    | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m)

  let certs_of_string str =
    let cs = Cstruct.of_string ~off:0 ~len:(String.length str) str in
    certs_of_cstruct cs

  let certs_of_pem path =
    try
      let pem = read_file path in
      certs_of_cstruct pem
    with Invalid_argument m -> Fmt.failwith "Certificates in %s: %s" path m

  let read_dir path =
    let cwd = Sys.getcwd () in
    Sys.readdir (Filename.concat cwd path) |> Array.to_list

  let certs_of_pem_dir path =
    read_dir path
    |> List.filter (fun file -> extension file = Some "crt")
    |> List.map (fun file -> certs_of_pem (Filename.concat path file))
    |> List.flatten

  let crl_of_pem path =
    try
      let data = read_file path in
      match X509.CRL.decode_der data with
      | Ok cs -> cs
      | Error (`Msg m) -> invalid_arg ("failed to parse CRL " ^ m)
    with Invalid_argument m -> Fmt.failwith "CRL in %s: %s" path m

  let crls_of_pem_dir path =
    read_dir path
    |> List.map (fun file -> crl_of_pem (Filename.concat path file))

  (* Would be better to take an Eio.Time.clock here, but that API is likely to change soon. *)
  let authenticator ?allowed_hashes ?crls param =
    let time () = Some (Ptime_clock.now ()) in
    let of_cas cas =
      let crls = Option.map crls_of_pem_dir crls in
      X509.Authenticator.chain_of_trust ?allowed_hashes ?crls ~time cas
    and dotted_hex_to_cs hex =
      Cstruct.of_hex (String.map (function ':' -> ' ' | x -> x) hex)
    and fingerp hash fingerprint =
      X509.Authenticator.server_key_fingerprint ~time ~hash ~fingerprint
    and cert_fingerp hash fingerprint =
      X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprint
    in
    match param with
    | `Ca_contents cs -> certs_of_string cs |> of_cas
    | `Ca_file path -> certs_of_pem path |> of_cas
    | `Ca_dir path -> certs_of_pem_dir path |> of_cas
    | `Key_fingerprint (hash, fp) -> fingerp hash fp
    | `Hex_key_fingerprint (hash, fp) ->
        let fp = dotted_hex_to_cs fp in
        fingerp hash fp
    | `Cert_fingerprint (hash, fp) -> cert_fingerp hash fp
    | `Hex_cert_fingerprint (hash, fp) ->
        let fp = dotted_hex_to_cs fp in
        cert_fingerp hash fp
end

module Auth = struct
  let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

  let make_default authenticator =
    let mypsk = ref None in

    let ticket_cache =
      {
        Tls.Config.lookup = (fun _ -> None);
        ticket_granted = (fun psk epoch -> mypsk := Some (psk, epoch));
        lifetime = 0l;
        timestamp = Ptime_clock.now;
      }
    in

    Tls.Config.(
      client ~version:(`TLS_1_0, `TLS_1_3) ?cached_ticket:!mypsk ~ticket_cache
        ~authenticator ~ciphers:Ciphers.supported ())

  let default () = make_default (X509.authenticator Ca_store.certificate)
  let null () = make_default (fun ?ip:_ ~host:_ _ -> Ok None)
end

module Tls_unix = struct
  exception Tls_alert of Tls.Packet.alert_type
  exception Tls_failure of Tls.Engine.failure

  type 'src t = {
    writer : 'src IO.Writer.t;
    reader : 'src IO.Reader.t;
    mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
    mutable linger : Cstruct.t option;
    recv_buf : IO.Buffer.t;
  }

  exception Unix_error of Unix.error

  let read t ~buf =
    match IO.Reader.read t.reader ~buf with
    | Ok n -> Ok n
    | Error (`Unix_error err) ->
        let exn = Unix_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn
    | Error err -> Error err

  exception Writer_closed

  let write t ~data =
    match IO.Writer.write t.writer ~data with
    | Ok n -> Ok n
    | Error `Closed -> raise Writer_closed
    | Error (`Unix_error err) ->
        let exn = Unix_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let cs_write t ~data =
    let data = IO.Buffer.of_cstruct ~filled:(Cstruct.length data) data in
    let _n = write t ~data |> Result.get_ok in
    ()

  let rec read_react t =
    Logger.debug (fun f -> f "read_react");
    let handle tls buf =
      Logger.debug (fun f ->
          f "handle: pos=%d fill=%d" (IO.Buffer.position buf)
            (IO.Buffer.filled buf));
      let cs = IO.Buffer.as_cstruct buf in
      match Tls.Engine.handle_tls tls cs with
      | Ok (state', `Response resp, `Data data) ->
          let state' =
            match state' with
            | `Ok tls -> `Active tls
            | `Eof -> `Eof
            | `Alert a -> `Error (Tls_alert a)
          in
          t.state <- state';
          Option.iter (fun data -> cs_write t ~data) resp;
          data
      | Error (alert, `Response resp) ->
          Logger.error (fun f ->
              f "tls_failure: %a" Tls.Engine.pp_failure alert);
          t.state <- `Error (Tls_failure alert);
          cs_write t ~data:resp;
          read_react t
    in

    match t.state with
    | `Error e -> raise e
    | `Eof -> raise End_of_file
    | `Active _ -> (
        Logger.debug (fun f -> f "read_react(active)");
        let n = read t ~buf:t.recv_buf |> Result.get_ok in
        Logger.debug (fun f -> f "read_react(active).read %d" n);
        match (t.state, n) with
        | `Active tls, n -> handle tls (IO.Buffer.sub t.recv_buf ~off:0 ~len:n)
        | `Error e, _ -> raise e
        | `Eof, _ -> raise End_of_file)

  let rec single_read t buf =
    Logger.debug (fun f -> f "single_read");
    let writeout res =
      let open Cstruct in
      let rlen = length res in
      let n = min (length buf) rlen in
      blit res 0 buf 0 n;
      t.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None);
      n
    in

    match t.linger with
    | Some res -> writeout res
    | None -> (
        Logger.debug (fun f -> f "no linger");
        match read_react t with
        | None -> single_read t buf
        | Some res -> writeout res)

  exception Tls_socket_closed

  let writev t css =
    match t.state with
    | `Error err -> raise err
    | `Eof -> raise Tls_socket_closed
    | `Active tls -> (
        match Tls.Engine.send_application_data tls css with
        | Some (tls, tlsdata) ->
            t.state <- `Active tls;
            cs_write t ~data:tlsdata
        | None -> invalid_arg "tls: write: socket not ready")

  let single_write t ~data =
    Logger.debug (fun f -> f "single_write");
    let cs = IO.Buffer.as_cstruct data in
    writev t [ cs ];
    Ok (Cstruct.lenv [ cs ])

  let rec drain_handshake t =
    let push_linger t mcs =
      match (mcs, t.linger) with
      | None, _ -> ()
      | scs, None -> t.linger <- scs
      | Some cs, Some l -> t.linger <- Some (Cstruct.append l cs)
    in
    match t.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> t
    | _ ->
        let cs = read_react t in
        push_linger t cs;
        drain_handshake t

  let client_of_fd config ?host fd =
    match
      let config' =
        match host with
        | None -> config
        | Some host -> Tls.Config.peer config host
      in
      let t =
        {
          state = `Eof;
          writer = Net.Socket.to_writer fd;
          reader = Net.Socket.to_reader fd;
          linger = None;
          recv_buf = IO.Buffer.with_capacity (4_096 * 1_024);
        }
      in
      let tls, init = Tls.Engine.client config' in
      let t = { t with state = `Active tls } in
      cs_write t ~data:init;
      drain_handshake t
    with
    | exception exn -> Error (`Tls_error exn)
    | t -> Ok t

  let to_reader : type src. src t -> src t IO.Reader.t =
   fun t ->
    let module Read = IO.Reader.Make (struct
      type nonrec t = src t

      let read t ~buf =
        let cs = IO.Buffer.as_cstruct buf in
        let len =
          match single_read t cs with exception End_of_file -> 0 | len -> len
        in
        IO.Buffer.set_filled buf ~filled:len;
        Ok len
    end) in
    IO.Reader.of_read_src (module Read) t

  let to_writer : type src. src t -> src t IO.Writer.t =
   fun t ->
    let module Write = IO.Writer.Make (struct
      type nonrec t = src t

      let write = single_write
      let flush _t = Ok ()
    end) in
    IO.Writer.of_write_src (module Write) t
end

type conn = {
  uri : Uri.t;
  addr : Net.Addr.stream_addr; [@warning "-69"]
  sock : Net.Socket.stream_socket; [@warning "-69"]
  writer : Net.Socket.stream_socket Tls_unix.t IO.Writer.t;
  reader : Net.Socket.stream_socket Tls_unix.t IO.Reader.t;
}

type request_ref = unit Ref.t

type response =
  [ `Status of Http.Status.t
  | `Headers of Http.Header.t
  | `Data of IO.Buffer.t
  | `Done ]

let req_to_buf uri (req : Http.Request.t) ?body () =
  let Http.Request.{ headers; meth; version; resource; _ } = req in
  let version = version |> Http.Version.to_string |> Httpaf.Version.of_string in

  let add_header name value headers =
    match value with
    | Some value -> Httpaf.Headers.add_unless_exists headers name value
    | None -> headers
  in

  let content_length =
    Option.map (fun b -> IO.Buffer.length b |> Int.to_string) body
  in

  let headers =
    headers |> Http.Header.to_list |> Httpaf.Headers.of_list
    |> add_header "content-length" content_length
    |> add_header "host" (Uri.host uri)
  in
  let meth = meth |> Http.Method.to_string |> Httpaf.Method.of_string in
  let req = Httpaf.Request.create ~version ~headers meth resource in
  let buf = Faraday.create (1024 * 1024) in
  Httpaf.Httpaf_private.Serialize.write_request buf req;
  Option.iter
    (fun body ->
      let cs = IO.Buffer.as_cstruct body in
      let ba = Cstruct.to_bigarray cs in
      Faraday.write_bigstring buf ~off:0 ~len:(IO.Buffer.length body) ba)
    body;
  let ba = Faraday.serialize_to_bigstring buf in
  let len = Bigstringaf.length ba in
  let cs = Cstruct.of_bigarray ~off:0 ~len ba in
  IO.Buffer.of_cstruct ~filled:len cs

let connect ?(auth = Auth.default ()) uri =
  let* addr =
    Net.Addr.of_uri uri |> Option.to_result ~none:(`Invalid_uri uri)
  in
  let* sock = Net.Socket.connect addr in
  let* tls = Tls_unix.client_of_fd auth sock in
  let reader = Tls_unix.to_reader tls in
  let writer = Tls_unix.to_writer tls in
  Ok { uri; addr; sock; reader; writer }

let request conn req ?body () =
  let req_ref = Ref.make () in
  let data = req_to_buf conn.uri req ?body () in
  let* _bytes = IO.Writer.write ~data conn.writer in
  Ok (conn, req_ref)

let stream conn =
  let buf = IO.Buffer.with_capacity (1024 * 2) in

  let state = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.response in
  let rec read state =
    match state with
    | Angstrom.Buffered.Partial continue ->
        let* _len = IO.Reader.read ~buf conn.reader in
        let cs = IO.Buffer.as_cstruct buf in
        let data = Cstruct.to_bigarray cs in
        let state = continue (`Bigstring data) in
        read state
    | Angstrom.Buffered.Done (prefix, res) ->
        let prefix =
          let cs =
            Cstruct.of_bigarray ~off:prefix.off ~len:prefix.len prefix.buf
          in
          IO.Buffer.of_cstruct ~filled:(Cstruct.length cs) cs
        in
        let content_length =
          "content-length"
          |> Httpaf.Headers.get Httpaf.Response.(res.headers)
          |> Option.map int_of_string
        in
        let need_to_read =
          content_length
          |> Option.map (fun cl -> cl - IO.Buffer.length prefix)
          |> Option.value ~default:(1024 * 10)
        in
        let buf = IO.Buffer.with_capacity need_to_read in
        let copied = IO.Buffer.copy ~src:prefix ~dst:buf in
        Logger.debug (fun f -> f "copied %d bytes" copied);
        let str = ref "" in
        let rec read_body () =
          let* len = IO.Reader.read ~buf conn.reader in
          let data = IO.Buffer.to_string buf in
          str := !str ^ data;
          Logger.debug (fun f -> f "read %d bytes: %S" len data);
          if String.ends_with ~suffix:"\n\n\r\n0\r\n\r\n" data then Ok ()
          else read_body ()
        in
        let* () = read_body () in
        let buf = IO.Buffer.of_string !str in
        Ok (buf, res)
    | Angstrom.Buffered.Fail _ -> Error `Response_parsing_error
  in
  let* body, res = read state in
  Ok
    ( conn,
      [
        `Status (res.status |> Httpaf.Status.to_code |> Http.Status.of_int);
        `Headers (res.headers |> Httpaf.Headers.to_list |> Http.Header.of_list);
        `Data body;
        `Done;
      ] )
