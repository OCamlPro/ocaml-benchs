open Core.Std
open Async.Std

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

let run () =
  Deferred.don't_wait_for 
    (Tcp.with_connection
       (Tcp.to_host_and_port "127.0.0.1" 8765)
       (fun _ r w -> 
          Log.Global.printf "(Client) Connected to server."; 
          send_stuff r w
          >>= fun() -> Reader.close r 
          >>= fun () -> Writer.close w 
          >>| fun () -> Shutdown.shutdown 0))    
    

let _ =
  run ();
  Scheduler.go ()
