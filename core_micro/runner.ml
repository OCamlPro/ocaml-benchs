open Core.Std
open Core_bench
module Entry = Pa_bench_lib.Benchmark_accumulator.Entry

let basic_run test =
  let Test.Basic_test.T f = test.Test.Basic_test.f in
  for i = 1 to 10_000 do
    ignore (f ())
  done

(* these two functions are from Inline_benchmarks.Runner *)
let make_benchmark_name entry =
  let module_name =
    match Entry.get_module_name_opt entry with
    | Some s -> ":" ^ s
    | None -> ""
  in
  let bench_module_name =
    match entry.Entry.bench_module_name with
    | Some s -> ":" ^ s
    | None -> ""
  in
  "[" ^ entry.Entry.filename
  ^ module_name
  ^ bench_module_name
  ^ "] " ^ entry.Entry.name
let entry_to_bench_test entry ~key =
  let open Entry in
  let name = make_benchmark_name entry in
  match entry.Entry.test_spec with
  | Regular_thunk f ->
       let func = f () in
       Test.create ~name ~key func
  | Indexed_thunk { arg_values; thunk; _ } ->
    Test.create_indexed
      ~name ~args:arg_values ~key
      (fun len -> Staged.stage (thunk len))

let () =
  let entries = Pa_bench_lib.Benchmark_accumulator.lookup_lib ~libname:"core" in
  let tests =
    List.map entries ~f:(fun entry ->
        entry_to_bench_test entry ~key:entry.Entry.unique_id)
  in
  List.iter ~f:basic_run (Test.expand tests)
