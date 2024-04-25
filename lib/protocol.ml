open Riot

module type Intf = sig
  module Request : sig
    val to_buffer :
      Uri.t -> Http.Request.t -> ?body:Bytestring.t -> unit -> Bytestring.t
  end

  module Response : sig
    val read_header :
      'src IO.Reader.t ->
      ( Http.Status.t * Http.Header.t * Bytestring.t,
        [> `Closed | `Eof | `Response_parsing_error | `Unix_error of Unix.error ]
      )
      IO.io_result

    val read_body :
      prefix:Bytestring.t ->
      headers:Http.Header.t ->
      body_remaining:int ->
      'a IO.Reader.t ->
      [> `Error of
         [> `Closed
         | `Connection_closed
         | `Eof
         | `Excess_body_read
         | `Exn of exn
         | `No_info
         | `Noop
         | `Process_down
         | `Timeout
         | `Unix_error of Unix.error
         | `Would_block ]
      | `More of Bytestring.t list * Bytestring.t * int
      | `Ok of Bytestring.t list ]
  end
end

module Http1 : Intf = Http1
module Http2 : Intf = Http2

module Negotiate = struct
  let of_uri uri =
    match Uri.scheme uri |> Option.map String.lowercase_ascii with
    | Some "https" -> (module Http2 : Intf)
    | Some "http" | _ -> (module Http1 : Intf)
end
