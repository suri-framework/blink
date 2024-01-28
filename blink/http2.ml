open Riot

module Request = struct
  let to_buffer _uri _req ?body:_ () = Bytestring.of_string "unimplemented"
end

module Response = struct
  let read_header _reader = Ok (`OK, Http.Header.of_list [], Bytestring.empty)
  let read_body ~prefix:_ ~headers:_ ~body_remaining:_ _reader = `Ok []
end
