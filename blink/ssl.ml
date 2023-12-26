(**
  The majority of this module has been taken verbatim (and in some places refactored to work with Riot)
  from `ocaml-tls` and its `eio` subpackage.
  
  Namely:
  * https://github.com/mirleft/ocaml-tls/blob/main/eio/tls_eio.ml
  * https://github.com/mirleft/ocaml-tls/blob/main/eio/x509_eio.ml

  See their license: https://github.com/mirleft/ocaml-tls/blob/main/LICENSE.md
*)

open Riot

let ( let* ) = Result.bind

module X509 = struct
  include X509

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

  let make_auth data =
    let time () = Some (Ptime_clock.now ()) in
    let d = "-----" in
    let new_cert = d ^ "BEGIN CERTIFICATE" ^ d
    and end_of_cert = d ^ "END CERTIFICATE" ^ d in
    let len_new = String.length new_cert
    and len_end = String.length end_of_cert in
    let lines = String.split_on_char '\n' data in
    let it, cas =
      List.fold_left
        (fun (acc, cas) line ->
          match acc with
          | None
            when String.length line >= len_new
                 && String.(equal (sub line 0 len_new) new_cert) ->
              (Some [ line ], cas)
          | None ->
              Logger.debug (fun m -> m "ignoring line %s" line);
              (None, cas)
          | Some lines
            when String.length line >= len_end
                 && String.(equal (sub line 0 len_end) end_of_cert) -> (
              let data = String.concat "\n" (List.rev (line :: lines)) in
              match X509.Certificate.decode_pem (Cstruct.of_string data) with
              | Ok ca -> (None, ca :: cas)
              | Error (`Msg msg) ->
                  Logger.warn (fun m ->
                      m "Failed to decode a trust anchor %s." msg);
                  Logger.debug (fun m -> m "Full certificate:@.%s" data);
                  (None, cas))
          | Some lines -> (Some (line :: lines), cas))
        (None, []) lines
    in
    (match it with
    | None -> ()
    | Some lines ->
        Logger.debug (fun m ->
            m "ignoring leftover data: %s" (String.concat "\n" (List.rev lines))));
    let cas = List.rev cas in
    match cas with
    | [] ->
        Logger.error (fun f -> f "ca-certs: empty trust anchors.");
        sleep 0.1;
        Stdlib.exit 1
    | _ -> X509.Authenticator.chain_of_trust ~time cas

  let make_default authenticator = Tls.Config.client ~authenticator ()
  let default () = make_default (make_auth Ca_store.pem)
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
    recv_buf : Cstruct.t;
  }

  exception Read_error of [ `Closed | `Eof | `Unix_error of Unix.error ]
  exception Write_error of [ `Closed | `Eof | `Unix_error of Unix.error ]

  let err_to_str err =
    match err with
    | `Closed -> "closed"
    | `Eof -> "eof"
    | `Unix_error err -> Unix.error_message err

  let read_t t cs =
    match IO.Reader.read t.reader ~buf:(IO.Buffer.of_cstruct cs) with
    | Ok n ->
        Logger.debug (fun f -> f "read_t: %d/%d" n (Cstruct.length cs));
        n
    | Error (`Closed | `Eof) ->
        Logger.debug (fun f -> f "read_t: 0/%d" (Cstruct.length cs));
        raise End_of_file
    | Error err ->
        Logger.debug (fun f -> f "read_t: error: %s" (err_to_str err));
        let exn = Read_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let write_t t cs =
    match IO.write_all t.writer ~data:(IO.Buffer.of_cstruct cs) with
    | Ok bytes ->
        Logger.debug (fun f -> f "write_t: %d/%d" bytes (Cstruct.length cs))
    | Error err ->
        Logger.debug (fun f -> f "write_t: error: %s" (err_to_str err));
        let exn = Write_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let try_write_t t cs =
    try write_t t cs with _ -> Logger.debug (fun f -> f "try_write_t failed")

  let rec read_react t =
    Logger.debug (fun f -> f "tls.read_react");
    let handle tls cs =
      match Tls.Engine.handle_tls tls cs with
      | Ok (state', `Response resp, `Data data) ->
          Logger.debug (fun f -> f "tls.read_react->ok");
          let state' =
            match state' with
            | `Ok tls -> `Active tls
            | `Eof -> `Eof
            | `Alert a ->
                Logger.debug (fun f -> f "tls.read_react->alert");
                `Error (Tls_alert a)
          in
          t.state <- state';
          Option.iter (try_write_t t) resp;
          data
      | Error (alert, `Response resp) ->
          Logger.debug (fun f -> f "tls.read_react->error");
          t.state <- `Error (Tls_failure alert);
          write_t t resp;
          read_react t
    in

    match t.state with
    | `Error e -> raise e
    | `Eof -> raise End_of_file
    | `Active _ -> (
        let n = read_t t t.recv_buf in
        match (t.state, n) with
        | `Active tls, n -> handle tls (Cstruct.sub t.recv_buf 0 n)
        | `Error e, _ -> raise e
        | `Eof, _ -> raise End_of_file)

  let rec single_read t buf =
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
        match read_react t with
        | None -> single_read t buf
        | Some res -> writeout res)

  exception Tls_socket_closed

  let writev t css =
    match t.state with
    | `Error err ->
        Logger.debug (fun f -> f "writev: failed");
        raise err
    | `Eof -> raise Tls_socket_closed
    | `Active tls -> (
        match Tls.Engine.send_application_data tls css with
        | Some (tls, tlsdata) ->
            t.state <- `Active tls;
            write_t t tlsdata
        | None -> invalid_arg "tls: write: socket not ready")

  let single_write t ~data =
    let cs = IO.Buffer.as_cstruct data in
    writev t [ cs ];
    let written = Cstruct.lenv [ cs ] in
    Ok written

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

  let make ?host ~reader ~writer config =
    let config' =
      match host with
      | None -> config
      | Some host -> Tls.Config.peer config host
    in
    let t =
      {
        state = `Eof;
        writer;
        reader;
        linger = None;
        recv_buf = Cstruct.create 4_096;
      }
    in
    let tls, init = Tls.Engine.client config' in
    let t = { t with state = `Active tls } in
    write_t t init;
    drain_handshake t

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

let connect addr uri =
  let auth = Auth.null () in
  let* sock = Net.Socket.connect addr in
  let reader, writer = (Net.Socket.to_reader sock, Net.Socket.to_writer sock) in
  let* host =
    let host = Uri.host_with_default ~default:"0.0.0.0" uri in
    let* domain_name = Domain_name.of_string host in
    Domain_name.host domain_name
  in
  let tls = Tls_unix.make ~host ~reader ~writer auth in
  let reader, writer = (Tls_unix.to_reader tls, Tls_unix.to_writer tls) in
  let conn = Connection.make ~reader ~writer ~addr ~uri in
  Ok conn
