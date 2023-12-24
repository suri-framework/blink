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
      Option.map (fun b -> IO.Buffer.length b |> Int.to_string) body
    in

    let headers =
      headers |> Http.Header.to_list |> Httpaf.Headers.of_list
      |> add_header "content-length" content_length
      |> add_header "host" (Uri.host uri)
    in
    let meth = meth |> Http.Method.to_string |> Httpaf.Method.of_string in
    let req = Httpaf.Request.create ~version ~headers meth resource in
    let buf = Faraday.create (1024 * 1024) in
    Httpaf.Httpaf_private.Serialize.write_request buf req;
    Option.iter
      (fun body ->
        let cs = IO.Buffer.as_cstruct body in
        let ba = Cstruct.to_bigarray cs in
        Faraday.write_bigstring buf ~off:0 ~len:(IO.Buffer.length body) ba)
      body;
    let ba = Faraday.serialize_to_bigstring buf in
    let len = Bigstringaf.length ba in
    let cs = Cstruct.of_bigarray ~off:0 ~len ba in
    IO.Buffer.of_cstruct ~filled:len cs
end

module Response = struct
  let of_reader reader ~buf =
    Logger.error (fun f -> f " Http1.Response.of_reader");
    let state = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.response in
    let rec read state =
      match state with
      | Angstrom.Buffered.Partial continue ->
          let* _len = IO.Reader.read ~buf reader in
          let cs = IO.Buffer.as_cstruct buf in
          let data = Cstruct.to_bigarray cs in
          let state = continue (`Bigstring data) in
          read state
      | Angstrom.Buffered.Done (prefix, res) ->
          let prefix =
            let cs =
              Cstruct.of_bigarray ~off:prefix.off ~len:prefix.len prefix.buf
            in
            IO.Buffer.of_cstruct ~filled:(Cstruct.length cs) cs
          in
          let content_length =
            "content-length"
            |> Httpaf.Headers.get Httpaf.Response.(res.headers)
            |> Option.map int_of_string
          in
          let need_to_read =
            content_length
            |> Option.map (fun cl -> cl - IO.Buffer.length prefix)
            |> Option.value ~default:(1024 * 10)
          in
          let buf = IO.Buffer.with_capacity need_to_read in
          let copied = IO.Buffer.copy ~src:prefix ~dst:buf in
          Logger.debug (fun f -> f "copied %d bytes" copied);
          let str = ref "" in
          let rec read_body () =
            let* len = IO.Reader.read ~buf reader in
            let data = IO.Buffer.to_string buf in
            str := !str ^ data;
            Logger.debug (fun f -> f "read %d bytes: %S" len data);
            if String.ends_with ~suffix:"\n\n\r\n0\r\n\r\n" data then Ok ()
            else read_body ()
          in
          let* () = read_body () in
          let buf = IO.Buffer.of_string !str in
          let parts =
            [
              `Status (res.status |> Httpaf.Status.to_code |> Http.Status.of_int);
              `Headers
                (res.headers |> Httpaf.Headers.to_list |> Http.Header.of_list);
              `Data buf;
              `Done;
            ]
          in
          Ok parts
      | Angstrom.Buffered.Fail _ -> Error `Response_parsing_error
    in
    read state
end
