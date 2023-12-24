(**
   {1 Blink}

   {@ocaml[
    let* conn = Blink.connect (Uri.parse "http://google.com") in
    let* conn, req_ref = 
      let req = Http.Request.make "/" in
      Blink.request ~conn ~req
    in
    let* parts = Blink.stream ~conn in
    ]}

*)

open Riot

type conn

val connect :
  ?auth:Tls.Config.client ->
  Uri.t ->
  ( conn,
    [> `Closed
    | `Invalid_uri of Uri.t
    | `Timeout
    | `Tls_error of exn
    | `Unix_error of Unix.error ] )
  IO.result

type request_ref

val request :
  conn ->
  Http.Request.t ->
  ?body:IO.Buffer.t ->
  unit ->
  (conn * request_ref, [> `Request_error | `Closed ]) IO.result

type response =
  [ `Status of Http.Status.t
  | `Headers of Http.Header.t
  | `Data of IO.Buffer.t
  | `Done ]

val stream :
  conn ->
  ( conn * response list,
    [> `Response_parsing_error | `Closed | `Eof ] )
  IO.result

module Auth : sig
  val default : unit -> Tls.Config.client
  val null : unit -> Tls.Config.client
end
