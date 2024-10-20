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
let* conn = Blink.connect url in
(* $MDX part-end *)
(* $MDX part-begin=request *)
let req = Http.Request.make "/" in
let* conn = Blink.request conn req () in
(* $MDX part-end *)
(* $MDX part-begin=stream *)
let* _conn, response, body = Blink.await conn in
(* $MDX part-end *)
match
  (response.status, Http.Header.to_list response.headers, Bytestring.length body)
with
| `OK, _, 74428 ->
    Logger.info (fun f -> f "get_ocaml_org_test: OK");
    sleep 0.1;
    Ok (shutdown ())
| _ ->
    Logger.error (fun f ->
        f "> Got response:\n%s\n%s\n%d\n%!"
          (Http.Status.to_string response.status)
          (Http.Header.to_lines response.headers |> String.concat "")
          (Bytestring.length body));
    (* Logger.error (fun f -> f "%a" Bytestring.pp body); *)
    sleep 0.1;
    Stdlib.exit 1
