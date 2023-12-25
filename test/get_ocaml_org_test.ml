[@@@warning "-8"]

let ( let* ) = Result.bind

open Riot;;

Riot.run @@ fun () ->
Result.get_ok
@@
let* _ = Logger.start () in
Logger.set_log_level (Some Info);

(* $MDX part-begin=conn *)
let url = Uri.of_string "https://ocaml.org" in
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
let req = Http.Request.make ~scheme:"https" "/" in
let* conn = Blink.request conn req () in
(* $MDX part-end *)
(* $MDX part-begin=stream *)
let _conn, [ `Status status; `Headers headers; `Data body; `Done ] =
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
(* $MDX part-end *)
match (status, Http.Header.to_list headers, IO.Buffer.length body) with
| `OK, _, 59825->
    Logger.info (fun f -> f "get_ocaml_org_test: OK");
    sleep 0.1;
    Ok (shutdown ())
| _ ->
    Logger.error (fun f ->
        f "> Got response:\n%s\n%s\n%d\n%!"
          (Http.Status.to_string status)
          (Http.Header.to_lines headers |> String.concat "")
          (IO.Buffer.length body));
    sleep 0.1;
    Stdlib.exit 1
