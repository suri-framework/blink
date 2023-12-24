open Riot

let ( let* ) = Result.bind

let connect addr uri =
  let* sock = Net.Socket.connect addr in
  let writer = Net.Socket.to_writer sock in
  let reader = Net.Socket.to_reader sock in
  let conn = Connection.make ~reader ~writer ~addr ~uri in
  Ok conn
