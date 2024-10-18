open Riot

open Logger.Make (struct
  let namespace = [ "blink"; "http1" ]
end)

let ( let* ) = Result.bind

module Request = struct
  let to_buffer uri (req : Http.Request.t) ?body () =
    let Http.Request.{ headers; meth; version; resource; _ } = req in
    let version =
      version |> Http.Version.to_string |> Httpaf.Version.of_string
    in

    let add_header name value headers =
      match value with
      | Some value -> Httpaf.Headers.add_unless_exists headers name value
      | None -> headers
    in

    let content_length =
      Option.map (fun b -> Bytestring.length b |> Int.to_string) body
    in

    let headers =
      headers |> Http.Header.to_list |> Httpaf.Headers.of_list
      |> add_header "content-length" content_length
      |> add_header "host" (Uri.host uri)
      |> add_header "user-agent" (Some "OCaml/5.1 Riot/0.0.8 Blink/0.0.1")
      |> add_header "connection" (Some "close")
    in
    let meth = meth |> Http.Method.to_string |> Httpaf.Method.of_string in
    let req = Httpaf.Request.create ~version ~headers meth resource in
    let buf = Faraday.create (1024 * 1024) in
    Httpaf.Httpaf_private.Serialize.write_request buf req;
    Option.iter
      (fun body -> Faraday.write_string buf (Bytestring.to_string body))
      body;
    let s = Faraday.serialize_to_string buf in
    Logger.trace (fun f -> f "Request: %S" s);
    Bytestring.of_string s
end

module Response = struct
  let rec split ?(left = {%b||}) str =
    match%b str with
    | {| "\r\n"::bytes, rest::bytes |} -> [ left; rest ]
    | {| c::bytes(1), rest::bytes |} -> split ~left:Bytestring.(left ^ c) rest
    | {| _ |} -> []

  let rec read_body ~prefix ~headers ~body_remaining reader =
    match Http.Header.get_transfer_encoding headers with
    | Http.Transfer.Chunked -> (
        debug (fun f -> f "reading chunked body");
        match read_chunked_body ~buffer:prefix reader with
        | Ok (body, buffer) when Bytestring.is_empty buffer ->
            debug (fun f -> f "read chunked_body: ok");
            `Ok (if Bytestring.is_empty body then [] else [ body ])
        | Ok (body, buffer) ->
            debug (fun f -> f "read chunked_body: more");
            `More ([ body ], buffer, body_remaining)
        | Error reason -> `Error reason)
    | _ -> (
        debug (fun f -> f "reading content-length body");
        match read_content_length_body reader prefix body_remaining with
        | Ok (body, buffer, body_remaining) ->
            debug (fun f ->
                f "read content_length body: body_remaning=%d buffer=%d"
                  body_remaining (Bytestring.length buffer));
            if body_remaining = 0 && Bytestring.length buffer = 0 then (
              debug (fun f -> f "read content_length body: ok");
              `Ok [ body ])
            else (
              debug (fun f -> f "read content_length body: more");
              `More ([ body ], buffer, body_remaining))
        | Error reason -> `Error reason)

  and read_chunked_body ~buffer reader =
    debug (fun f -> f "buffer %S" (Bytestring.to_string buffer));
    match split buffer with
    | [ zero; _ ] when String.equal (Bytestring.to_string zero) "0" ->
        debug (fun f -> f "read_chunked_body: last chunk!");
        Ok (Bytestring.empty, Bytestring.empty)
    | [ chunk_size; chunk_data ] -> (
        debug (fun f ->
            f "[%S;%S]"
              (Bytestring.to_string chunk_size)
              (Bytestring.to_string chunk_data));
        let chunk_size =
          Int64.(of_string ("0x" ^ Bytestring.to_string chunk_size) |> to_int)
        in
        debug (fun f -> f "read_chunked_body: chunk_size=%d" chunk_size);
        let binstr_data = Bytestring.to_string chunk_data in
        debug (fun f ->
            f "read_chunked_body: (%d bytes)" (String.length binstr_data));
        let binstr_data = binstr_data |> Bitstring.bitstring_of_string in
        match%bitstring binstr_data with
        | {| full_chunk : (chunk_size * 8) : string ;
             "\r\n" : 2 * 8 : string ;
             rest : -1 : bitstring |}
          ->
            debug (fun f -> f "read_chunked_body: read full chunk");
            debug (fun f ->
                f "read_chunked_body: rest=%d" (Bitstring.bitstring_length rest));
            let rest =
              Bytestring.of_string (Bitstring.string_of_bitstring rest)
            in
            let full_chunk = Bytestring.of_string full_chunk in
            Ok (full_chunk, rest)
        | {| _ |} ->
            let left_to_read = chunk_size - Bytestring.length chunk_data + 2 in
            debug (fun f ->
                f "read_chunked_body: reading more data left_to_read=%d"
                  left_to_read);
            let* chunk = read ~to_read:left_to_read reader in
            let buffer = Bytestring.join buffer chunk in
            read_chunked_body ~buffer reader)
    | _ ->
        debug (fun f -> f "read_chunked_body: need more data");
        let* chunk = Bytestring.with_bytes (fun buf -> IO.read reader buf) in
        let buffer = Bytestring.join buffer chunk in
        read_chunked_body ~buffer reader

  and read_content_length_body reader buffer body_remaining =
    let limit = body_remaining in
    let to_read = limit - Bytestring.length buffer in
    debug (fun f ->
        f "read_content_length_body: up to limit=%d with preread_buffer=%d"
          limit (Bytestring.length buffer));
    match body_remaining with
    | 0 when Bytestring.length buffer >= limit ->
        debug (fun f -> f "read_content_length_body: can answer with buffer");
        (* let len = Int.min limit (Bytestring.length buffer) in *)
        (* let body = Bytestring.sub ~off:0 ~len buffer in *)
        Ok (buffer, Bytestring.empty, 0)
    | n when n < 0 || to_read < 0 ->
        debug (fun f -> f "read_content_length_body: excess body");
        Error `Excess_body_read
    | remaining_bytes ->
        let to_read =
          Int.min (limit - Bytestring.length buffer) remaining_bytes
        in
        debug (fun f -> f "read_content_length_body: need to read %d" to_read);
        let* chunk = read ~to_read reader in
        let body = Bytestring.join buffer chunk in
        let body_remaining = remaining_bytes - Bytestring.length body in
        Ok (body, Bytestring.empty, body_remaining)

  and read ~to_read ?(buffer = Bytestring.empty) reader =
    if to_read = 0 then Ok buffer
    else
      let buf = Bytes.create to_read in
      debug (fun f -> f "reading to_read=%d" to_read);
      let* len = IO.read reader buf in
      let chunk =
        Bytestring.of_string (Bytes.unsafe_to_string (Bytes.sub buf 0 len))
      in
      let remaining_bytes = to_read - Bytestring.length chunk in
      let buffer = Bytestring.join buffer chunk in
      debug (fun f -> f "read: remaining_bytes %d" remaining_bytes);
      debug (fun f -> f "read: buffer=%d" (Bytestring.length buffer));
      if remaining_bytes > 0 then read ~to_read:remaining_bytes ~buffer reader
      else Ok buffer

  let read_header reader =
    Logger.trace (fun f -> f " Http1.Response.of_reader");
    let state = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.response in
    let rec read state =
      match state with
      | Angstrom.Buffered.Partial continue ->
          let* data = Bytestring.with_bytes (fun buf -> IO.read reader buf) in
          let state = continue (`String (Bytestring.to_string data)) in
          read state
      | Angstrom.Buffered.Done (prefix, res) ->
          let prefix =
            let Angstrom.Buffered.{ buf; off; len } = prefix in
            Bigstringaf.sub buf ~off ~len
            |> Bigstringaf.to_string |> Bytestring.of_string
          in

          let status =
            Httpaf.Response.(res.status)
            |> Httpaf.Status.to_code |> Http.Status.of_int
          in

          let headers =
            Httpaf.Response.(res.headers)
            |> Httpaf.Headers.to_list |> Http.Header.of_list
          in

          Ok (status, headers, prefix)
      | Angstrom.Buffered.Fail _ -> Error `Response_parsing_error
    in
    read state
end
