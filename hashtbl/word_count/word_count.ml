(* Simply counts the number of
   occurences of each word in a text file *)

type 'a sequence = ('a -> unit) -> unit

let lines : in_channel -> string sequence = fun f ->
  let rec aux k = k (input_line f); aux k in
  fun k ->
  try aux k
  with End_of_file -> ()

let split : string -> string sequence = fun s k ->
  let n = String.length s in
  let rec white i =
    if i = n then
      ()
    else if s.[i] = ' ' || s.[i] = '\t' then
      white (i+1)
    else
      black i i
  and black i0 i =
    if i = n then
      k (String.sub s i0 (i-i0))
    else if s.[i] <> ' ' && s.[i] <> '\t' then
      black i0 (i+1)
    else begin
	k (String.sub s i0 (i-i0));
	white i
      end
  in
  white 0

let flatMap f seq k = seq (fun x -> f x k)

let count_file f =
  let t = Hashtbl.create ~random:false 1_000 in
  let get s = try Hashtbl.find t s with Not_found -> 0
  and set s n = Hashtbl.replace t s n
  and words = f |> lines |> flatMap split in
  words (fun s -> set s (get s +1))


let _ =
  let f = open_in "data.txt" in
  ignore @@ count_file f;
  close_in_noerr f
