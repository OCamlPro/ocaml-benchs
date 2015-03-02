open Core.Std
open Async.Std

(* A command that sends the hello request  *)
let say_hello host ~port =
  Rpc_common.with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_protocol.hello_rpc conn "Hi"
    >>| fun response ->
    if !Rpc_common.debug then printf "%s \n%!" response
  )
  ~host ~port

let plus_one host ~port i =
  Rpc_common.with_rpc_conn (fun conn ->
    Rpc.Rpc.dispatch_exn Rpc_protocol.plus_one_rpc conn i
    >>| fun response ->
    if !Rpc_common.debug then printf "%i\n%!" response
  )
  ~host ~port

let sum host ~port =
  Random.init 42;
  let len = Random.int 26 in
  let x = Random.int 10 in
  let arr = Array.create ~len x in
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.sum_rpc conn arr
      >>| fun response ->
      if !Rpc_common.debug then printf "%i\n%!" response
    )
    ~host ~port

let add host ~port x y =
  let ti = { Rpc_common.a = x; Rpc_common.b = y } in
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.add_rpc conn ti
      >>| fun response ->
      if !Rpc_common.debug then printf "%i\n%!" response;
    )
    ~host ~port

let all_query host ~port =
  Random.init 42;
  let len = Random.int 26 in
  let x = Random.int 10 in
  let arr = Array.create ~len x in
  let ti = { Rpc_common.a = 8; Rpc_common.b = 9 } in
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.hello_rpc conn "Hi"
      >>= fun response ->
      if !Rpc_common.debug then printf "%s \n%!" response;
      Rpc.Rpc.dispatch_exn Rpc_protocol.plus_one_rpc conn 5
      >>= fun response ->
      if !Rpc_common.debug then printf "%i\n%!" response;
      Rpc.Rpc.dispatch_exn Rpc_protocol.sum_rpc conn arr
      >>= fun response ->
      if !Rpc_common.debug then printf "%i\n%!" response;
      Rpc.Rpc.dispatch_exn Rpc_protocol.add_rpc conn ti
      >>| fun response ->
      if !Rpc_common.debug then printf "%i\n%!" response
    )
    ~host ~port

let record_query host ~port record = 
  Rpc_common.with_rpc_conn (fun conn ->
      Rpc.Rpc.dispatch_exn Rpc_protocol.record_rpc conn record
      >>| fun response ->
      if !Rpc_common.debug then printf "%s \n%!" response.Rpc_common.str
    )
    ~host ~port
    
let run host ~port =
  all_query host ~port

let run_nbr ~port nbr =
  let record = { Rpc_common.i = 5;
                 Rpc_common.i64 = Int64.zero;
                 Rpc_common.f= 3.14;
                 Rpc_common.str = "test";
                 Rpc_common.b = false;
                 Rpc_common.farr = [42.0; 42.0];
                 Rpc_common.some = Rpc_common.A 3;
                 Rpc_common.iopt = Some 42 } 
  in
  let host = "127.0.0.1" in
  let rec iter i =
    if i = nbr then return ()
    else 
      begin
        record_query host ~port record
        >>= fun () -> iter (i + 1)
      end
  in
  iter 0
