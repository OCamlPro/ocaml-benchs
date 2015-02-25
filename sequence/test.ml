open Sequence

(* For testing, we create a sequence which is equal to 1;2;3;4;5, but
   with a more interesting structure inside*)

let s12345 = map ~f:(fun x -> x / 2) (filter ~f:(fun x -> x mod 2 = 0)
                                        (of_list [1;2;3;4;5;6;7;8;9;10]))

let sempty = filter ~f:(fun x -> x < 0) (of_list [1;2;3;4])

let test f g = test_to_list s12345 f g && test_to_list sempty f g

let s = range 0 100_000

let do_stuff_1 n =
  let s = range 0 n in
  fold ~f:(+) ~init:0
    (map ~f:(fun x -> x * 2) s)

let do_stuff_2 n =
  let s = range 0 n in
  fold ~f:(+) ~init:0
    (filter ~f:(fun x -> x mod 2 = 0)
       (map ~f:(fun x -> x * 2) s))

let () =
  let n = int_of_string Sys.argv.(1) in
  let r = ref 0 in
  for i = 0 to n do
    r := !r + do_stuff_1 i;
  done;
  for i = 0 to n do
    r := !r + do_stuff_2 i;
  done;
  print_int !r;
  print_newline ()

let () =
  try
    let fn = Sys.getenv "OCAML_GC_STATS" in
    let oc = open_out fn in
    Gc.print_stat oc
  with _ -> ()
