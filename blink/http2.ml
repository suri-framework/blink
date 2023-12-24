open Riot

module Request = struct
  let to_buffer _uri _req ?body:_ () = IO.Buffer.of_string "unimplemented"
end

module Response = struct
  let of_reader _reader ~buf:_ = Ok []
end
