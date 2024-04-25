open Riot

module type Intf = sig
  val name : string

  val connect :
    Net.Addr.stream_addr ->
    Uri.t ->
    ( Connection.t,
      [> `Closed
      | `Unix_error of Unix.error
      | `Tls_error of exn
      | `Msg of string ] )
    IO.io_result
end

module Tcp : Intf
module Ssl : Intf

module Negotiator : sig
  val of_uri : Uri.t -> (module Intf)
end
