[@@@warning "-8"]

let ( let* ) = Result.bind

open Riot;;

Riot.run @@ fun () ->
(fun result ->
  match result with
  | Ok _ -> ()
  | Error (`Application_error s) ->
      Logger.error (fun f -> f "error: application error %S" s)
  | Error (`Invalid_uri _) -> Logger.error (fun f -> f "error: invalid_uri")
  | Error (`Msg s) -> Logger.error (fun f -> f "error: %S" s)
  | Error `Supervisor_error ->
      Logger.error (fun f -> f "error: supervisor error")
  | Error `Excess_body_read ->
      Logger.error (fun f -> f "error: Excess_body_read")
  | Error `Response_parsing_error ->
      Logger.error (fun f -> f "error: response parsing error")
  | Error (`Unknown_opcode _) ->
      Logger.error (fun f -> f "error: unknown opcode error")
  | Error (`Tls_error _e) -> Logger.error (fun f -> f "error: tls error")
  | Error (#IO.io_error as err) ->
      Logger.error (fun f -> f "error: %a" IO.pp_err err))
@@
let* _ = Logger.start () in
Logger.set_log_level (Some Trace);

(* $MDX part-begin=conn *)
let url = Uri.of_string "http://0.0.0.0:8080" in
let* conn = Blink.connect url in
(* $MDX part-end *)
(* $MDX part-begin=request *)
let* sock = Blink.WebSocket.upgrade conn "/ws" in
(* $MDX part-end *)
(* $MDX part-begin=stream *)
let hello_world = Blink.Frame.text "hello world" in

Logger.info (fun f -> f "hello_world: %a" Blink.Frame.pp hello_world);

let* sock = Blink.WebSocket.send [ hello_world ] sock in
let* _sock, [ frame ] = Blink.WebSocket.receive sock in
Logger.info (fun f -> f "got frame: %a" Blink.Frame.pp frame);
(* $MDX part-end *)
Ok ()
