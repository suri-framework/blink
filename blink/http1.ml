open Riot

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
      |> add_header "user-agent" (Some "OCaml/Blink")
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
  let of_reader reader =
    Logger.trace (fun f -> f " Http1.Response.of_reader");
    let state = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.response in
    let rec read state =
      match state with
      | Angstrom.Buffered.Partial continue ->
          let* data = Bytestring.with_bytes (fun buf -> IO.read ~buf reader) in
          let state = continue (`String (Bytestring.to_string data)) in
          read state
      | Angstrom.Buffered.Done (prefix, res) ->
          let content_length =
            "content-length"
            |> Httpaf.Headers.get Httpaf.Response.(res.headers)
            |> Option.map int_of_string
          in

          let prefix =
            let cs =
              Cstruct.of_bigarray ~off:prefix.off ~len:prefix.len prefix.buf
            in
            Bytestring.of_string (Cstruct.to_string cs)
          in
          let _need_to_read =
            content_length
            |> Option.map (fun cl -> cl - Bytestring.length prefix)
            |> Option.value ~default:(1024 * 10)
          in

          let str = ref prefix in
          let rec read_body () =
            let* data =
              Bytestring.with_bytes (fun buf -> IO.read ~buf reader)
            in
            (str := Bytestring.(!str ^ data));
            Logger.debug (fun f ->
                f "read bytes: %S" (Bytestring.to_string data));
            if
              Bytestring.length data = 0
              || String.ends_with ~suffix:"\n\n\r\n0\r\n\r\n"
                   (Bytestring.to_string data)
            then Ok ()
            else read_body ()
          in
          let* () = read_body () in
          let parts =
            [
              `Status (res.status |> Httpaf.Status.to_code |> Http.Status.of_int);
              `Headers
                (res.headers |> Httpaf.Headers.to_list |> Http.Header.of_list);
              `Data !str;
              `Done;
            ]
          in
          Ok parts
      | Angstrom.Buffered.Fail _ -> Error `Response_parsing_error
    in
    read state
end
