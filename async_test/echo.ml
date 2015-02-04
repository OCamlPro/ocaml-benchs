open Core.Std
open Async.Std
       
let log addr input = 
  Log.Global.printf "%i bytes from (%s)" 
    (String.length input)
    (Async_extra.Import.Socket.Address.to_string addr);
  input

let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun addr r w ->
         Pipe.transfer (Reader.pipe r) (Writer.pipe w) ~f:(log addr))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () =
  run ();
  never_returns (Scheduler.go ())
