open Core.Std
open Async.Std

let log addr input = 
  Log.Global.printf "(Server) received %i bytes from (%s)" 
    (String.length input)
    (Async_extra.Import.Socket.Address.to_string addr);
  input

let run_server () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun addr r w ->
         Log.Global.printf "(Server) Connection from (%s)" 
           (Async_extra.Import.Socket.Address.to_string addr);
         Pipe.transfer (Reader.pipe r) (Writer.pipe w) ~f:(log addr))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let send_stuff r w = 
  Random.init 42;
  let rec send_1000_stuff i =
    if i = 1000 then return ()
    else
      let len = (Random.int 26) + 1 in
      let s = String.make len 'a' in
      Writer.write w s;
      Log.Global.printf "(Client-%i) send '%s'." i s;
      let buffer = String.create len in
      Reader.read r buffer
      >>= function
      | `Eof -> return ()
      | `Ok _ ->
        Log.Global.printf "(Client-%i) received '%s'." i buffer;
        send_1000_stuff (i + 1)
  in
  send_1000_stuff 0

let run_client () =
  Deferred.don't_wait_for 
    (Tcp.with_connection
       (Tcp.to_host_and_port "127.0.0.1" 8765)
       (fun _ r w -> 
          Log.Global.printf "(Client) Connected to server."; 
          send_stuff r w
          >>= fun() -> Reader.close r 
          >>= fun () -> Writer.close w 
          (* >>| fun () -> Shutdown.shutdown 0 *)
       ))  

let _ =
  run_server ();
  run_client ();
  never_returns (Scheduler.go ())
