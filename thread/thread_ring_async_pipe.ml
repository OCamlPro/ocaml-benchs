open Core.Std
open Async.Std

let pred = function
  | 0 -> 502
  | n -> pred n

let succ = function
  | 502 -> 0
  | n -> succ n

let make_th pipes n id =
  let rec do_n_times = function
    | 0 -> Deferred.unit
    | n ->
      Pipe.read @@ fst pipes.(pred id) >>= fun _ ->
      Pipe.write (snd pipes.(id)) () >>= fun () ->
      do_n_times (pred n)
  in
  do_n_times n

let main n =
  let pipes = Array.init 503 ~f:(fun _ -> Pipe.create ()) in
  let ths = Array.init 503 ~f:(make_th pipes n)
  in
  Pipe.write_without_pushback (snd pipes.(0)) ();
  ths.(502) >>= fun () ->
  (try
     Sys.getenv "OCAML_GC_STATS" |> function
     | Some fn -> Out_channel.with_file fn ~f:(fun oc -> Gc.print_stat oc)
     | _ -> ()
   with _ -> ());
  Shutdown.exit 0

let () =
  let n = if Array.length Sys.argv > 1
    then int_of_string Sys.argv.(1)
    else 50000
  in
  ignore @@ main n;
  never_returns @@ Scheduler.go ()
