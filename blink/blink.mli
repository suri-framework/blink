(** {1 Blink}

Blink is a low-level, functional client for HTTP and WebSockets.

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

(**
   [stream conn] is a low-level API that returns a list of messages streamed
   from the connection and a handler to the updated connection.

   You can use this to process data as it comes
*)
val stream :
  Connection.t ->
  ( Connection.t * Connection.message list,
    [> `Closed
    | `Eof
    | `Response_parsing_error
    | `Excess_body_read
    | `Unix_error of Unix.error ] )
  IO.io_result

(**
   [messages conn] will stream and collect all messages coming in from a
   connection. This is handy if you want to consume messages in batches.
*)
val messages :
  Connection.t ->
  ( Connection.t * Connection.message list,
    [> `Closed
    | `Eof
    | `Response_parsing_error
    | `Excess_body_read
    | `Unix_error of Unix.error ] )
  IO.io_result

(**
   [await conn] will stream the connection until it is drained, collect the
   body, and build a response object.

   Useful for one-off request/response cycles.

   For stream processing of body responses see [stream conn].
*)
val await : Connection.t -> 
  ( Connection.t * Http.Response.t * Bytestring.t,
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
