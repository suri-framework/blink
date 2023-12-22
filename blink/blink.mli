(**
   {1 Blink}

   {@ocaml[
    let* conn = Blink.connect (Uri.parse "http://google.com") in
    let* conn, req_ref = 
      let req = Http.Request.make "/" in
      Blink.request ~conn ~req
    in
    let* parts = Blink.stream ~conn in
    ]}

*)

type conn

type nonrec ('ok, 'err) result =
  ('ok, ([> `Closed | `Unix_error of Unix.error | `Timeout ] as 'err)) result

val connect : Uri.t -> (conn, [> `Invalid_uri of Uri.t ]) result

type request_ref

val request :
  conn ->
  Http.Request.t ->
  ?body:Bigstringaf.t ->
  unit ->
  (conn * request_ref, [> `Request_error ]) result

type response =
  [ `Status of Http.Status.t
  | `Headers of Http.Header.t
  | `Data of Bigstringaf.t
  | `Done ]

val stream : conn -> (response list, [> `Response_parsing_error ]) result
