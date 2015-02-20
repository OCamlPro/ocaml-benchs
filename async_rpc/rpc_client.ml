open Core.Std
open Async.Std

(* A command that sends the hello request  *)
let say_hello ~host ~port =
  Rpc_common.with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_protocol.hello_rpc conn "Hi"
    >>| fun response ->
    printf "%s \n%!" response
  )
    ~host ~port

let plus_one ~host ~port i =
  Rpc_common.with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_protocol.plus_one_rpc conn i
    >>| fun response ->
    printf "%i\n%!" response
  )
    ~host ~port

let sum ~host ~port =
  Random.init 42;
  let len = Random.int 26 in
  let x = Random.int 10 in
  let arr = Array.create ~len x in
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.sum_rpc conn arr
      >>| fun response ->
      printf "%i\n%!" response
    )
    ~host ~port

let add ~host ~port x y =
  let ti = { Rpc_common.a = x; Rpc_common.b = y } in
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.add_rpc conn ti
      >>| fun response ->
      printf "%i\n%!" response
    )
    ~host ~port

let command i =
  Command.async_basic
    ~summary:"Rpc client"
    Command.Spec.(
      empty
      +> Rpc_common.port_arg ()
      +> Rpc_common.host_arg ()
    )
    (fun port host () -> 
       say_hello ~port ~host 
       >>= fun () -> plus_one ~port ~host i
       >>= fun () -> sum ~port ~host
       >>= fun () -> add ~port ~host 5 6
    )

let command_plus i =
  Command.async_basic
    ~summary:"Plus One client"
    Command.Spec.(
      empty
      +> Rpc_common.port_arg ()
      +> Rpc_common.host_arg ()
    )
    (fun port host () -> plus_one ~port ~host i)

let () = 
  Command.run (command 3)
