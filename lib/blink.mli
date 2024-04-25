(** {1 Blink}

*)

open Riot
module Frame = Frame

(** {2 Connection}

*)
module Connection : sig
  type t

  type message =
    [ `Data of Bytestring.t
    | `Done
    | `Headers of Http.Header.t
    | `Status of Http.Status.t ]
end

val pp_messages : Format.formatter -> Connection.message list -> unit

val connect :
  Uri.t ->
  ( Connection.t,
    [> `Closed
    | `Invalid_uri of Uri.t
    | `Tls_error of exn
    | `Unix_error of Unix.error
    | `Msg of string ] )
  IO.io_result

val request :
  Connection.t ->
  Http.Request.t ->
  ?body:Bytestring.t ->
  unit ->
  (Connection.t, [> `Closed | `Unix_error of Unix.error ]) IO.io_result

val stream :
  Connection.t ->
  ( Connection.t * Connection.message list,
    [> `Closed
    | `Eof
    | `Response_parsing_error
    | `Excess_body_read
    | `Unix_error of Unix.error ] )
  IO.io_result

module WebSocket : sig
  type t

  val upgrade :
    Connection.t ->
    string ->
    ( t,
      [> `Closed
      | `Eof
      | `Response_parsing_error
      | `Excess_body_read
      | `Unknown_opcode of int
      | `Msg of string
      | `Unix_error of Unix.error ] )
    IO.io_result

  val send :
    Frame.t list ->
    t ->
    (t, [> `Msg of string | `Unknown_opcode of int ]) IO.io_result

  val receive :
    t ->
    ( t * Frame.t list,
      [> `Msg of string | `Unknown_opcode of int ] )
    IO.io_result
end
