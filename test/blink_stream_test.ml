[@@@warning "-8"]

let ( let* ) = Result.bind

open Riot

let () =
  Riot.run @@ fun () ->
  let _ = Logger.start () in
  Logger.set_log_level (Some Info);
  let run () =
    let open Stdlib.Result in
    let* conn = Blink.connect @@ Uri.of_string "https://api.scryfall.com" in
    let req = Http.Request.make "/sets/eld" in
    let* conn = Blink.request conn req () in
    let* conn, [ _; _ ] = Blink.stream conn in
    let* conn, [ `Data body ] = Blink.stream conn in
    let* _conn, [ `Done ] = Blink.stream conn in
    let str = Bytestring.to_string body in
    Format.printf "%s\n%!" str;
    Ok ()
  in
  run () |> Result.get_ok
