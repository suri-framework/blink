open Riot

[@@@warning "-8"]

let ( let* ) = Result.bind

let main () =
  let _ = Logger.start () |> Result.get_ok in
  Logger.set_log_level (Some Info);
  let url = Sys.argv.(1) in
  let* conn = Blink.connect (Uri.of_string url) in
  let req = Http.Request.make "/" in
  let* conn, _req_ref = Blink.request conn req () in
  let* _conn, [ `Status status; `Headers headers; `Data body; `Done ] =
    Blink.stream conn
  in
  Format.printf "%s\n%s\n%s\n%!"
    (Http.Status.to_string status)
    (Http.Header.to_lines headers |> String.concat "")
    (Bigstringaf.to_string body);
  Ok (shutdown ())

let () = Riot.run @@ fun () -> main () |> Result.get_ok
