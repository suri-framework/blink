open Riot

[@@@warning "-8"]

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  (* $MDX part-begin=conn *)
  let url = Uri.of_string "http://ocaml.org" in
  let conn = Blink.connect url |> Result.get_ok in
  (* $MDX part-end *)
  (* $MDX part-begin=request *)
  let req = Http.Request.make "/" in
  let conn, _req_ref = Blink.request conn req () |> Result.get_ok in
  (* $MDX part-end *)
  (* $MDX part-begin=stream *)
  let _conn, [ `Status status; `Headers headers; `Data body; `Done ] =
    Blink.stream conn |> Result.get_ok
  in
  (* $MDX part-end *)
  match (status, Http.Header.to_list headers, Bigstringaf.to_string body) with
  | `Permanent_redirect, _, _ ->
      Logger.info (fun f -> f "get_ocaml_org_test: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f ->
          f "> Got response:\n%s\n%s\n%s\n%!"
            (Http.Status.to_string status)
            (Http.Header.to_lines headers |> String.concat "")
            (Bigstringaf.to_string body));
      sleep 0.1;
      Stdlib.exit 1
