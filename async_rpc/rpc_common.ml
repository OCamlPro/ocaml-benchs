open Core.Std
open Async.Std

type two_int = { a:int; b:int } with bin_io

type t1 = A of int | B of float | C of string with bin_io

type record = {
  i:int;
  i64:int64;
  f:float;
  str:string;
  b:bool;
  farr:float list;
  some:t1;
  iopt:int option
} with bin_io

let debug = ref false

let debug_arg () =
  Command.Spec.(
    flag "-debug" no_arg ~doc: " Print debug infos"
  )

let port_arg () =
  Command.Spec.(
    flag "-port" (optional_with_default 8124 int)
      ~doc:" Broker's port"
  )

let nbr_arg () =
  Command.Spec.(
    flag "-nbr" (optional_with_default 10 int)
      ~doc:" Number of client's query"
  )

let with_rpc_conn f ~host ~port =
  Tcp.with_connection
    (Tcp.to_host_and_port host port)
    ~timeout:(sec 1.)
    (fun _ r w ->
      Rpc.Connection.create ~connection_state:(fun _ -> ()) r w
      >>= function
      | Error exn -> raise exn
      | Ok conn   -> f conn
    )

let start_server ~implementations ~port run_client nbr =
  if !debug then Log.Global.info "Starting server on %d" port;
  let implementations =
    Rpc.Implementations.create_exn ~implementations
      ~on_unknown_rpc:(`Call (fun ~rpc_tag ~version ->
        Log.Global.info "Unexpected RPC, tag %s, version %d" rpc_tag version;
        `Close_connection))
  in
  Tcp.Server.create
    ~on_handler_error:(`Call (fun _ exn -> Log.Global.sexp exn Exn.sexp_of_t))
    (Tcp.on_port port)
    (fun _addr r w ->
      Rpc.Connection.server_with_close r w
        ~connection_state:(fun _ -> ())
        ~on_handshake_error:(
          `Call (fun exn -> Log.Global.sexp exn Exn.sexp_of_t; return ()))
        ~implementations
    )
  >>= fun server ->
  if !debug then Log.Global.info "Server started, waiting for close";
  (* Run Client *)
  run_client ~port nbr
  >>= fun () -> Tcp.Server.close server
