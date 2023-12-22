open Riot

let ( let* ) = Result.bind

type nonrec ('ok, 'err) result =
  ('ok, ([> `Closed | `Unix_error of Unix.error | `Timeout ] as 'err)) result

type conn = {
  uri : Uri.t;
  addr : Net.Addr.stream_addr; [@warning "-69"]
  sock : Net.Socket.stream_socket;
}

type request_ref = unit Ref.t

type response =
  [ `Status of Http.Status.t
  | `Headers of Http.Header.t
  | `Data of Bigstringaf.t
  | `Done ]

let req_to_buf uri (req : Http.Request.t) ?body () =
  let Http.Request.{ headers; meth; version; resource; _ } = req in
  let version = version |> Http.Version.to_string |> Httpaf.Version.of_string in

  let add_header name value headers =
    match value with
    | Some value -> Httpaf.Headers.add_unless_exists headers name value
    | None -> headers
  in

  let content_length =
    Option.map (fun b -> Bigstringaf.length b |> Int.to_string) body
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
      Faraday.write_bigstring buf ~off:0 ~len:(Bigstringaf.length body) body)
    body;
  Faraday.serialize_to_bigstring buf

let connect uri =
  let* addr =
    Net.Addr.of_uri uri |> Option.to_result ~none:(`Invalid_uri uri)
  in
  let* sock = Net.Socket.connect addr in
  Ok { uri; addr; sock }

let request conn req ?body () =
  let req_ref = Ref.make () in
  let data = req_to_buf conn.uri req ?body () in
  let* _bytes = Net.Socket.send ~data conn.sock in
  Ok (conn, req_ref)

let stream conn =
  let buf = Bigstringaf.create (1024 * 1024) in

  let state = Angstrom.Buffered.parse Httpaf.Httpaf_private.Parse.response in
  let rec read state =
    match state with
    | Angstrom.Buffered.Partial continue ->
        let* len = Net.Socket.receive ~buf conn.sock in
        let data = Bigstringaf.sub buf ~off:0 ~len in
        let state = continue (`Bigstring data) in
        read state
    | Angstrom.Buffered.Done (body, res) ->
        let body = Bigstringaf.sub body.buf ~off:body.off ~len:body.len in
        Ok (body, res)
    | Angstrom.Buffered.Fail _ -> Error `Response_parsing_error
  in
  let* body, res = read state in
  Ok
    ( conn,
      [
        `Status (res.status |> Httpaf.Status.to_code |> Http.Status.of_int);
        `Headers (res.headers |> Httpaf.Headers.to_list |> Http.Header.of_list);
        `Data body;
        `Done;
      ] )
