(* Simply counts the number of
   occurences of each word in a text file *)

let split =
  let r = Str.regexp "[ 	]" in
  fun s -> Str.split_delim r s

let count_file f =
  let t = Hashtbl.create ~random:false 1_000 in
  let get s = try Hashtbl.find t s with Not_found -> 0
  and set s n = Hashtbl.replace t s n in
  begin try
      while true do
	input_line f |> split |> List.iter (fun s -> set s (get s +1))
      done
    with End_of_file -> ()
  end;
  t

let _ =
  let f = open_in "data.txt" in
  ignore @@ count_file f;
  close_in_noerr f
