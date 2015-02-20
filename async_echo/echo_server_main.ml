open Core.Std
open Async.Std

let command = 
  Command.async_basic
    ~summary:"Echo Server"
    Command.Spec.(
      empty 
      +> Echo_common.port_arg ()
      +> Echo_common.debug_arg ()
    )
    (fun port debug () -> 
       Echo_common.debug := debug; 
       Echo_server.run ~port None ())

let () = Command.run command
