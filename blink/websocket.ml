let ( let* ) = Result.bind

type t = Connection.t

let upgrade conn =
  (* let nonce = "" in *)
  (* let extension = [] in *)

  (* let req = Http.Request.make ~meth:`GET *)
  (* let* conn = Connection.request conn req () in *)

  (* let* messages = Connection.messages conn  in  *)
  Ok conn

let send _frame conn = Ok conn
let receive conn = Ok (conn, [])
