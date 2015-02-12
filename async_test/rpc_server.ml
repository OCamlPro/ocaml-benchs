open Core.Std
open Async.Std

(* The implementation of the "hello" RPC.  The first argument is the environment
   the query executes against, which in this case is trivial.

   The RPC waits a 10th of a second before responding just to show how you do a
   query whose implementation blocks.
*)
let hello_impl () hello =
  Log.Global.info "received hello query (%s)" hello;
  Clock.after (sec 0.1)
  >>= fun () ->
  return (hello ^ " World!")

let plus_impl () i =
  Log.Global.info "received plus query (%i)" i;
  Clock.after (sec 0.1)
  >>= fun () ->
  return (i + 1)

let sum_impl () arr =
  Log.Global.info "received sum query";
  let sum = Array.fold arr ~init:0 ~f:(fun acc x -> x + acc) in
  Clock.after (sec 0.1)
  >>= fun () ->
  return (sum)

let add_impl () ti =
   Log.Global.info "received add query (%i + %i)" ti.Rpc_common.a ti.Rpc_common.b;
  Clock.after (sec 0.1)
  >>= fun () ->
  return (ti.a + ti.b)

(* The list of RPC implementations supported by this server *)
let implementations =
  [ Rpc.Rpc.implement Rpc_protocol.hello_rpc hello_impl;
    Rpc.Rpc.implement Rpc_protocol.plus_one_rpc plus_impl;
    Rpc.Rpc.implement Rpc_protocol.sum_rpc sum_impl;
    Rpc.Rpc.implement Rpc_protocol.add_rpc add_impl ]

(* The command-line interface.  We use [async_basic] so that the command starts
   the async scheduler, and exits when the server stops.  *)
let command =
  Command.async_basic
    ~summary:"Hello World server"
    Command.Spec.(
      empty +> Rpc_common.port_arg ()
    )
    (fun port () -> Rpc_common.start_server ~port ~implementations ())

let () = Command.run command
