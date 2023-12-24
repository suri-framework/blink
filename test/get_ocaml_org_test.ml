open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Debug);
  (* $MDX part-begin=conn *)
  let url = Uri.of_string "https://github.com" in
  let conn =
    match Blink.connect url with
    | Ok conn -> conn
    | Error err ->
        (match err with
        | `Closed -> Logger.error (fun f -> f "closed")
        | `Invalid_uri uri ->
            Logger.error (fun f -> f "invalid_uri %a" Uri.pp uri)
        | `Tls_error exn ->
            Logger.error (fun f -> f "tls_error %s" (Printexc.to_string exn))
        | `Timeout -> Logger.error (fun f -> f "timeout")
        | `Unix_error err ->
            Logger.error (fun f -> f "unix_error: %s" (Unix.error_message err)));
        sleep 0.1;
        Stdlib.exit 1
  in
  (* $MDX part-end *)
  (* $MDX part-begin=request *)
  let req = Http.Request.make "/" in
  let conn, _req_ref = Blink.request conn req () |> Result.get_ok in
  (* $MDX part-end *)
  (* $MDX part-begin=stream *)
  let _conn, parts =
    match Blink.stream conn with
    | Ok x -> x
    | Error err ->
        (match err with
        | `Eof | `Closed -> Logger.error (fun f -> f "closed")
        | `Response_parsing_error ->
            Logger.error (fun f -> f "response_parsing_error")
        | `Invalid_uri uri ->
            Logger.error (fun f -> f "invalid_uri %a" Uri.pp uri)
        | `Tls_error exn ->
            Logger.error (fun f -> f "tls_error %s" (Printexc.to_string exn))
        | `Timeout -> Logger.error (fun f -> f "timeout")
        | `Unix_error err ->
            Logger.error (fun f -> f "unix_error: %s" (Unix.error_message err)));
        sleep 0.1;
        Stdlib.exit 1
  in

  let[@warning "-8"] [ `Status status; `Headers headers; `Data body; `Done ] =
    parts
  in

  (* $MDX part-end *)
  match (status, Http.Header.to_list headers, IO.Buffer.length body) with
  | `OK, _, 0 ->
      Logger.info (fun f -> f "get_ocaml_org_test: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f ->
          f "> Got response:\n%s\n%s\n%s\n%!"
            (Http.Status.to_string status)
            (Http.Header.to_lines headers |> String.concat "")
            (IO.Buffer.to_string body));
      sleep 0.1;
      Stdlib.exit 1
