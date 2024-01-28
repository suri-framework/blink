[@@@warning "-8"]

let ( let* ) = Result.bind

open Riot;;

Riot.run @@ fun () ->
Result.get_ok
@@
let* _ = Logger.start () in
Logger.set_log_level (Some Info);

(* $MDX part-begin=conn *)
let url = Uri.of_string "wss://bazaar.fly.dev/ws" in
let* conn = Blink.connect url in
(* $MDX part-end *)
(* $MDX part-begin=request *)
let* sock = Blink.WebSocket.upgrade conn in
(* $MDX part-end *)
(* $MDX part-begin=stream *)
let hello_world = Trail.Frame.text "hello world!" in
let* sock = Blink.WebSocket.send hello_world sock in
let* _sock, [ frame ] = Blink.WebSocket.receive sock in
Logger.info (fun f -> f "got frame: %a" Trail.Frame.pp frame)
(* $MDX part-end *)
