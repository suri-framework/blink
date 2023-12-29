open Riot

let ( let* ) = Result.bind

module type Intf = sig
  val connect :
    Net.Addr.stream_addr ->
    Uri.t ->
    ( Connection.t,
      [> `Closed
      | `Unix_error of Unix.error
      | `Tls_error of exn
      | `Msg of string ] )
    result
end

module Tcp : Intf = struct
  let connect addr uri =
    let* sock = Net.Socket.connect addr in
    let writer = Net.Socket.to_writer sock in
    let reader = Net.Socket.to_reader sock in
    let conn = Connection.make ~reader ~writer ~addr ~uri in
    Ok conn
end

module Ssl : Intf = struct
  module Auth = struct
    let () =
      Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

    let make_default authenticator = Tls.Config.client ~authenticator ()

    let default () =
      let time () = Some (Ptime_clock.now ()) in
      let decode_pem ca =
        let ca = Cstruct.of_string ca in
        let cert = X509.Certificate.decode_pem ca in
        Result.get_ok cert
      in
      let cas = List.map decode_pem Ca_store.certificates in
      let authenticator = X509.Authenticator.chain_of_trust ~time cas in
      make_default authenticator

    [@@@warning "-32"]

    let null () = make_default (fun ?ip:_ ~host:_ _ -> Ok None)
  end

  let connect addr uri =
    let auth = Auth.default () in
    let* sock = Net.Socket.connect addr in
    let* host =
      let host = Uri.host_with_default ~default:"0.0.0.0" uri in
      let* domain_name = Domain_name.of_string host in
      Domain_name.host domain_name
    in
    let reader, writer = SSL.of_socket ~host ~auth sock in
    let conn = Connection.make ~reader ~writer ~addr ~uri in
    Ok conn
end

module Negotiator = struct
  let of_uri uri =
    match Uri.scheme uri |> Option.map String.lowercase_ascii with
    | Some "https" -> (module Ssl : Intf)
    | Some "http" | _ -> (module Tcp : Intf)
end
