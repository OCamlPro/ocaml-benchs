open Core.Std
open Async.Std

(* The implementation of the "hello" RPC.  The first argument is the environment
   the query executes against, which in this case is trivial.

   The RPC waits a 10th of a second before responding just to show how you do a
   query whose implementation blocks.
*)
let hello_impl () hello =
  if !Rpc_common.debug then Log.Global.info "received hello query (%s)" hello;
  return (hello ^ " World!")

let plus_impl () i =
  if !Rpc_common.debug then Log.Global.info "received plus query (%i)" i;
  return (i + 1)

let sum_impl () arr =
  if !Rpc_common.debug then Log.Global.info "received sum query";
  let sum = Array.fold arr ~init:0 ~f:(fun acc x -> x + acc) in
  return (sum)

let add_impl () ti =
  if !Rpc_common.debug then Log.Global.info "received add query (%i + %i)" ti.Rpc_common.a ti.Rpc_common.b;
  return (ti.Rpc_common.a + ti.Rpc_common.b)

let record_impl () record =
  if !Rpc_common.debug then Log.Global.info "received rec query (%s)" record.Rpc_common.str;
  return record

(* The list of RPC implementations supported by this server *)
let implementations =
  [ Rpc.Rpc.implement Rpc_protocol.hello_rpc hello_impl;
    Rpc.Rpc.implement Rpc_protocol.plus_one_rpc plus_impl;
    Rpc.Rpc.implement Rpc_protocol.sum_rpc sum_impl;
    Rpc.Rpc.implement Rpc_protocol.add_rpc add_impl;
    Rpc.Rpc.implement Rpc_protocol.record_rpc record_impl
  ]

(* The command-line interface.  We use [async_basic] so that the command starts
   the async scheduler, and exits when the server stops.  *)
let command =
  Command.async_basic
    ~summary:"Rpc server"
    Command.Spec.(
      empty 
      +> Rpc_common.port_arg ()
      +> Rpc_common.nbr_arg ()
      +> Rpc_common.debug_arg ()
    )
    (fun port nbr debug () -> 
       Rpc_common.debug := debug; 
       Rpc_common.start_server ~port ~implementations Rpc_client.run_nbr nbr)

let () = Command.run command
