open Core.Std
open Async.Std

let command = 
  Command.async_basic
    ~summary:"Echo client"
    Command.Spec.(
      empty 
      +> Echo_common.port_arg ()
      +> Echo_common.nbr_arg ()
      +> Echo_common.debug_arg ()
    )
    (fun port nbr debug () -> Echo_common.debug := debug; Echo_client.run ~port ~nbr ())

let () = Command.run command
  
