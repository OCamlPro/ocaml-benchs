open Core.Std
open Async.Std

let yield_th n =
  let n = Random.int n in
  let rec inner = function
    | 0 -> return ()
    | n -> Scheduler.yield () >>= fun () -> inner (pred n)
  in inner n

let main nb_thread nb_yield =
  let rec th_list acc = function
    | 0 -> acc
    | n -> th_list ((yield_th nb_yield)::acc) (pred n)
  in
  Deferred.all_ignore @@ th_list [] nb_thread >>= fun () ->
  (try
     Sys.getenv "OCAML_GC_STATS" |> function
     | Some fn -> Out_channel.with_file fn ~f:(fun oc -> Gc.print_stat oc)
     | _ -> ()
   with _ -> ());
  Shutdown.exit 0

let () =
  let nb_args = Array.length Sys.argv - 1 in
  let nb_thread = if nb_args < 1 then 1_000_000 else int_of_string Sys.argv.(1) in
  let nb_yield = if nb_args < 2 then 10 else int_of_string Sys.argv.(2) in
  ignore @@ main nb_thread nb_yield;
  never_returns @@ Scheduler.go ()
