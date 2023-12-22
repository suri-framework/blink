open Riot

[@@@warning "-8"]

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let conn =
    Blink.connect (Uri.of_string "http://ocaml.org") |> Result.get_ok
  in
  let headers = Http.Header.of_list [ ("Host", "ocaml.org") ] in
  let req = Http.Request.make ~headers "/" in
  let conn, _req_ref = Blink.request conn req () |> Result.get_ok in
  let [ `Status status; `Headers headers; `Data body; `Done ] =
    Blink.stream conn |> Result.get_ok
  in
  match (status, Http.Header.to_list headers, Bigstringaf.to_string body) with
  | `Permanent_redirect, _, _ ->
      Logger.info (fun f -> f "get_ocaml_org_test: OK");
      sleep 0.1;
      shutdown ()
  | _ ->
      Logger.error (fun f-> f "> Got response:\n%s\n%s\n%s\n%!"
        (Http.Status.to_string status)
        (Http.Header.to_lines headers |> String.concat "")
        (Bigstringaf.to_string body));
      sleep 0.1;
      Stdlib.exit 1
