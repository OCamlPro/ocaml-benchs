open Core.Std
open Async.Std

let p_s = function
  | 0 -> 502, 1
  | 502 -> 501, 0
  | n -> pred n, succ n

let make_th p s n id =
  let rec do_n_times = function
    | 0 -> return ()
    | n ->
      Pipe.transfer_id (fst p) (snd s) >>= fun () ->
      do_n_times (pred n)
  in
  do_n_times n

let main n =
  let pipes = Array.init 503
      ~f:(fun _ -> Pipe.create ()) in
  let ths = Array.init 503
      ~f:(fun id ->
          let p, s = p_s id in
          let p, s = pipes.(p), pipes.(s) in
          make_th p s n id)
  in
  Pipe.write_without_pushback (snd pipes.(0)) ();
  ths.(502) >>= fun () ->
  Shutdown.exit 0

let () =
  let n = if Array.length Sys.argv > 1
    then int_of_string Sys.argv.(1)
    else 50000
  in
  ignore @@ main n;
  never_returns @@ Scheduler.go ()
