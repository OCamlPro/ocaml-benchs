module Step = struct
  (* 'a is an item in the sequence, 's is the state that will produce the remainder of
     the sequence *)
  type ('a,'s) t =
    | Done
    | Skip of 's
    | Yield of 'a * 's
end

open Step

(* 'a is an item in the sequence, 's is the state that will produce the remainder of the
   sequence *)
type +_ t =
  | Sequence : 's * ('s -> ('a,'s) Step.t) -> 'a t

type 'a sequence = 'a t

let unfold_step ~init ~f =
  Sequence (init,f)

let unfold ~init ~f =
  unfold_step ~init
    ~f:(fun s ->
      match f s with
      | None -> Step.Done
      | Some(a,s) -> Step.Yield(a,s))

let unfold_with s ~init ~f =
  match s with
  | Sequence(s, next) ->
    Sequence((init, s) ,
             (fun (seed, s) ->
               match next s with
               | Done -> Done
               | Skip s -> Skip (seed, s)
               | Yield(a,s) ->
                 match f seed a with
                 | Done -> Done
                 | Skip seed -> Skip (seed, s)
                 | Yield(a,seed) -> Yield(a,(seed,s))))

let of_list l =
  unfold_step ~init:l
    ~f:(function
        | [] -> Done
        | x::l -> Yield(x,l))


let fold t ~init ~f =
  let rec loop seed v next f =
    match next seed with
    | Done -> v
    | Skip s -> loop s v next f
    | Yield(a,s) -> loop s (f v a) next f
  in
  match t with
  | Sequence(seed, next) -> loop seed init next f

let to_list_rev t =
      fold t ~init:[] ~f:(fun l x -> x::l)

let to_list (Sequence(s,next)) =
  let safe_to_list t =
    List.rev (to_list_rev t)
  in
  let rec to_list s next i =
    if i = 0 then safe_to_list (Sequence(s,next))
    else
    match next s with
    | Done -> []
    | Skip s -> to_list s next i
    | Yield(a,s) -> a::(to_list s next (i-1))
  in
  to_list s next 500

let range ?(stride=1) ?(start=`inclusive) ?(stop=`exclusive) start_v stop_v =
  let step =
    match stop with
    | `inclusive when stride >= 0 ->
      fun i -> if i > stop_v then Done else Yield(i, i + stride)
    | `inclusive ->
      fun i -> if i < stop_v then Done else Yield(i, i + stride)
    | `exclusive when stride >= 0 ->
      fun i -> if i >= stop_v then Done else Yield(i,i + stride)
    | `exclusive ->
      fun i -> if i <= stop_v then Done else Yield(i,i + stride)
  in
  let init =
    match start with
    | `inclusive -> start_v
    | `exclusive -> start_v + stride
  in
  unfold_step ~init ~f:step

(* Functions used for testing by comparing to List implementation*)
let test_to_list s f g =
  to_list (f s) = g (to_list s)

let map t ~f =
  match t with
  | Sequence(seed, next) ->
    Sequence(seed,
             fun seed ->
               match next seed with
               | Done -> Done
               | Skip s -> Skip s
               | Yield(a,s) -> Yield(f a,s))

let mapi t ~f =
  match t with
  | Sequence(s, next) ->
    Sequence((0,s),
              fun (i,s) ->
               match next s with
               | Done -> Done
               | Skip s -> Skip (i,s)
               | Yield(a,s) -> Yield(f i a,(i+1,s)))

let filter t ~f =
  match t with
  | Sequence(seed, next) ->
    Sequence(seed,
             fun seed ->
               match next seed with
               | Done -> Done
               | Skip s -> Skip s
               | Yield(a,s) when f a -> Yield(a,s)
               | Yield (_,s) -> Skip s)

let filteri t ~f =
  map ~f:snd (
  filter (mapi t ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s))

let length t =
  let rec loop i s next =
    match next s with
    | Done -> i
    | Skip s -> loop i s next
    | Yield(_,s) -> loop (i+1) s next
  in
  match t with
  | Sequence (seed, next) -> loop 0 seed next

let to_list_rev_with_length t =
      fold t ~init:([],0) ~f:(fun (l,i) x -> (x::l,i+1))

let to_array t =
  let (l,len) = to_list_rev_with_length t in
  match l with
  | [] -> [||]
  | x::l ->
    let a = Array.make len x in
    let rec loop i l =
      match l with
      | [] -> assert (i = -1)
      | x::l -> a.(i) <- x; loop (i-1) l
    in
    loop (len - 2) l;
    a

let find t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield(a,_) when f a -> Some a
    | Yield(_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

let find_map t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield(a,s) ->
      (match f a with
       | None -> loop s next f
       | some_b -> some_b)
    | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

let for_all t ~f =
  let rec loop s next f =
    match next s with
    | Done -> true
    | Yield(a,_) when not (f a) -> false
    | Yield (_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

let exists t ~f =
  let rec loop s next f =
    match next s with
    | Done -> false
    | Yield(a,_) when f a -> true
    | Yield(_,s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f

let iter t ~f =
  let rec loop seed next f =
    match next seed with
    | Done -> ()
    | Skip s -> loop s next f
    | Yield(a,s) ->
      begin
        f a;
        loop s next f
      end
  in
  match t with
  | Sequence(seed, next) -> loop seed next f

let is_empty t =
  let rec loop s next =
    match next s with
    | Done -> true
    | Skip s -> loop s next
    | Yield _ -> false
  in
  match t with
  | Sequence(seed, next) -> loop seed next

let mem ?(equal = (=)) t a =
  let rec loop s next a =
    match next s with
    | Done -> false
    | Yield(b,_) when equal a b -> true
    | Yield(_,s) | Skip s -> loop s next a
  in
  match t with
  | Sequence(seed, next) -> loop seed next a

let empty =
  Sequence((), fun () -> Done)

let bind t f =
  unfold_step
    ~f:(function
      | Sequence(seed,next), rest ->
        match next seed with
        | Done ->
          begin
            match rest with
            | Sequence(seed, next) ->
              match next seed with
              | Done -> Done
              | Skip s -> Skip (empty, Sequence(s, next))
              | Yield(a, s) -> Skip(f a, Sequence(s, next))
          end
        | Skip s -> Skip (Sequence(s,next), rest)
        | Yield(a,s) -> Yield(a, (Sequence(s,next) , rest)))
    ~init:(empty,t)

let return x =
  unfold_step ~init:(Some x)
    ~f:(function
      | None -> Done
      | Some x -> Yield(x,None))

let nth s n =
  if n < 0 then None
  else
    let rec loop i s next =
      match next s with
      | Done -> None
      | Skip s -> loop i s next
      | Yield(a,s) -> if i == 0 then Some a else loop (i-1) s next
    in
    match s with
    | Sequence(s,next) ->
      loop n s next

let nth_exn s n =
  if n < 0 then raise (Invalid_argument "Core.Sequence.nth")
  else
  match nth s n with
  | None -> failwith "Sequence.nth"
  | Some x -> x

let merge (Sequence (s1, next1)) (Sequence (s2, next2)) ~cmp =
  let next = function
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | (s1, Skip s2) -> Skip (s1, next2 s2)
    | (Yield (a, s1') as s1), (Yield (b, s2') as s2) ->
      if cmp a b <= 0
      then Yield (a, (Skip s1', s2))
      else Yield (b, (s1, Skip s2'))
    | Done, Done -> Done
    | Yield (a, s1), Done -> Yield (a, (Skip s1, Done))
    | Done, Yield (b, s2) -> Yield (b, (Done, Skip s2))
  in
  Sequence((Skip s1, Skip s2), next)

let hd s =
 let rec loop s next =
   match next s with
   | Done -> None
   | Skip s -> loop s next
   | Yield(a,_) -> Some a
 in
 match s with
 | Sequence (s,next) -> loop s next

let hd_exn s =
  match hd s with
  | None -> failwith "hd_exn"
  | Some a -> a

let tl s =
 let rec loop s next =
   match next s with
   | Done -> None
   | Skip s -> loop s next
   | Yield(_,a) -> Some a
 in
 match s with
 | Sequence (s,next) ->
  match loop s next with
    | None -> None
    | Some s -> Some (Sequence(s,next))

let tl_eagerly_exn s =
  match tl s with
  | None -> failwith "Sequence.tl_exn"
  | Some s -> s

let lift_identity next s =
  match next s with
  | Done -> Done
  | Skip s -> Skip (`Identity s)
  | Yield(a,s) -> Yield(a, `Identity s)

let next s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip s -> loop s next
    | Yield(a,s) -> Some (a, Sequence(s, next))
  in
  match s with
  | Sequence(s, next) -> loop s next

let filter_opt s =
  match s with
  | Sequence(s, next) ->
    Sequence(s,
      fun s ->
      match next s with
      | Done -> Done
      | Skip s -> Skip s
      | Yield(None, s) -> Skip s
      | Yield(Some a, s) -> Yield(a, s))

let filter_map s ~f =
  filter_opt (map s ~f)

let filter_mapi s ~f =
  filter_map (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun (i, s) -> f i s)

let split_n_eagerly s n =
  let rec loop s i accum next =
    if i <= 0 then
      (of_list (List.rev accum), Sequence(s,next))
    else
      match next s with
      | Done -> (of_list (List.rev accum), empty)
      | Skip s -> loop s i accum next
      | Yield(a,s) -> loop s (i-1) (a::accum) next
  in
  match s with
  | Sequence(s, next) -> loop s n [] next

let findi s ~f =
  find (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s)

let find_exn s ~f =
  match find s ~f with
  | None -> failwith "Sequence.find_exn"
  | Some x -> x

let append s1 s2 =
  match s1, s2 with
  | Sequence(s1, next1), Sequence(s2, next2) ->
    Sequence(`First_list s1,
             function
             | `First_list s1 ->
                begin
                match next1 s1 with
                  | Done -> Skip (`Second_list s2)
                  | Skip s1 -> Skip (`First_list s1)
                  | Yield(a,s1) -> Yield(a, `First_list s1)
                end
             | `Second_list s2 ->
                begin
                match next2 s2 with
                  | Done -> Done
                  | Skip s2 -> Skip (`Second_list s2)
                  | Yield(a,s2) -> Yield(a, `Second_list s2)
                end)

let concat_map s ~f = bind s f

let concat_mapi s ~f =
  concat_map (mapi s  ~f:(fun i s -> (i,s)))
    ~f:(fun (i,s) -> f i s)

let zip (Sequence (s1, next1)) (Sequence (s2, next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield ((a, b), (Skip s1, Skip s2))
    | Done, _
    | _, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
  in
  Sequence ((Skip s1, Skip s2), next)

let zip_full (Sequence(s1,next1)) (Sequence(s2,next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield (`Both (a, b), (Skip s1, Skip s2))
    | Done, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
    | Done, Yield(b, s2) -> Yield((`Right b), (Done, next2 s2))
    | Yield(a, s1), Done -> Yield((`Left a), (next1 s1, Done))
  in
  Sequence ((Skip s1, Skip s2), next)

let bounded_length (Sequence(seed,next)) ~at_most =
  let rec loop i seed next =
    if i > at_most then `Greater
    else
      match next seed with
      | Done -> `Is i
      | Skip seed -> loop i seed next
      | Yield(_, seed) -> loop (i+1) seed next
  in
  loop 0 seed next

let length_is_bounded_by ?(min=(-1)) ?max t =
  let length_is_at_least (Sequence(s,next)) =
     let rec loop s acc =
       if acc >= min then true else
         match next s with
         | Done -> false
         | Skip s -> loop s acc
         | Yield(_,s) -> loop s (acc + 1)
     in loop s 0
  in
  match max with
    | None -> length_is_at_least t
    | Some max ->
      begin
        match bounded_length t ~at_most:max with
        | `Is len when len >= min -> true
        | _ -> false
      end

let iteri s ~f =
  iter (mapi s ~f:(fun i s -> (i, s)))
    ~f:(fun (i, s) -> f i s)

let foldi s ~f ~init =
  fold ~init (mapi s ~f:(fun i s -> (i,s)))
    ~f:(fun acc (i, s) -> f i acc s)

let reduce s ~f =
  match next s with
  | None -> None
  | Some(a, s) -> Some (fold s ~init:a ~f)

let reduce_exn s ~f =
  match reduce s ~f with
  | None -> failwith "Sequence.reduce_exn"
  | Some res -> res

let find_consecutive_duplicate (Sequence(s, next)) ~equal =
  let rec loop last_elt s =
    match next s with
    | Done -> None
    | Skip s -> loop last_elt s
    | Yield(a,s) ->
      match last_elt with
      | Some b when equal a b -> Some (b, a)
      | None | Some _ -> loop (Some a) s
  in
  loop None s

let remove_consecutive_duplicates s ~equal =
  unfold_with s ~init:None
    ~f:(fun prev a ->
          match prev with
          | Some b when equal a b -> Skip(Some a)
          | None | Some _ -> Yield(a, Some a))

let count s ~f =
  length (filter s ~f)

let init n ~f =
  unfold_step ~init:0
    ~f:(fun i ->
      if i >= n then Done
      else Yield(f i, i + 1))

let sub s ~pos ~len =
  if pos < 0 || len < 0 then failwith "Sequence.sub";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                 if i - pos >= len then Done
                 else
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) when i >= pos -> Yield (a,(i + 1, s))
                   | Yield(_, s) -> Skip(i + 1, s)))

let take s len =
  if len < 0 then failwith "Sequence.take";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                 if i >= len then Done
                 else
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) -> Yield (a,(i + 1, s))))

let drop s len =
  if len < 0 then failwith "Sequence.drop";
  match s with
  | Sequence(s, next) ->
    Sequence((0,s),
              (fun (i, s) ->
                   match next s with
                   | Done -> Done
                   | Skip s -> Skip (i, s)
                   | Yield(a, s) when i >= len -> Yield (a,(i + 1, s))
                   | Yield(_, s) -> Skip (i+1, s)))

let take_while s ~f =
  match s with
  | Sequence(s, next) ->
    Sequence(s,
             fun s ->
              match next s with
              | Done -> Done
              | Skip s -> Skip s
              | Yield (a, s) when f a -> Yield(a,s)
              | Yield (_,_) -> Done)

let drop_while s ~f =
  match s with
  | Sequence(s, next) ->
    Sequence(`Dropping s,
             function
             |`Dropping s ->
               begin
                match next s with
                | Done -> Done
                | Skip s -> Skip (`Dropping s)
                | Yield(a, s) when f a -> Skip (`Dropping s)
                | Yield(a, s) -> Yield(a, `Identity s)
               end
             | `Identity s -> lift_identity next s)

let shift_right s x =
  match s with
  | Sequence(seed, next) ->
    Sequence(`Consing (seed, x),
             function
               | `Consing (seed, x) -> Yield(x, `Identity seed)
               | `Identity s -> lift_identity next s)

let shift_right_with_list s l =
  append (of_list l) s

let shift_left = drop

module Infix = struct
  let (@) = append
end

let intersperse s ~sep =
  match s with
  | Sequence(s, next) ->
    Sequence(`Init s,
             function
             | `Init s ->
               begin
                 match next s with
                 | Done -> Done
                 | Skip s -> Skip (`Init s)
                 | Yield(a, s) -> Yield(a, `Running s)
               end
             | `Running s ->
               begin
                 match next s with
                 | Done -> Done
                 | Skip s -> Skip (`Running s)
                 | Yield(a, s) -> Yield(sep, `Putting(a,s))
               end
             | `Putting(a,s) -> Yield(a,`Running s))

let repeat x =
  unfold_step ~init:x ~f:(fun x -> Yield(x, x))

let cycle s =
  concat_map ~f:(fun () -> s) (repeat ())

let cartesian_product sa sb =
  concat_map sa
    ~f:(fun a -> zip (repeat a) sb)

let singleton x = return x

let delayed_fold s ~init ~f ~finish =
  let rec loop s next finish f =
    fun acc ->
      match next s with
      | Done   -> finish acc
      | Skip s ->  loop s next finish f acc
      | Yield(a, s) -> f acc a ~k:(loop s next finish f)
  in
  match s with
  | Sequence(s, next) -> loop s next finish f init

let force_eagerly t = of_list (to_list t)

let memoize (type a) (Sequence (s, next)) =
  let module M = struct
    type t = T of (a, t) Step.t Lazy.t
  end in
  let rec memoize s = M.T (lazy (find_step s))
  and find_step s =
    match next s with
    | Done -> Done
    | Skip s -> find_step s
    | Yield (a, s) -> Yield (a, memoize s)
  in
  Sequence (memoize s, (fun (M.T l) -> Lazy.force l))

let drop_eagerly s len =
  let rec loop i ~len s next =
    if i >= len then Sequence(s, next)
    else
      match next s with
      | Done -> empty
      | Skip s -> loop i ~len s next
      | Yield(_,s) -> loop (i+1) ~len s  next
  in
  match s with
  | Sequence(s, next) -> loop 0 ~len s next

let drop_while_option (Sequence (s, next)) ~f =
  let rec loop s =
    match next s with
    | Done -> None
    | Skip s -> loop s
    | Yield (x, s) -> if f x then loop s else Some (x, Sequence (s, next))
  in
  loop s

module Generator = struct

  type 'elt steps = Wrap of ('elt, unit -> 'elt steps) Step.t

  let unwrap (Wrap step) = step

  module T = struct
    type ('a, 'elt) t = ('a -> 'elt steps) -> 'elt steps
    let return x = (); fun k -> k x
    let bind m f = (); fun k -> m (fun a -> let m' = f a in m' k)
    let map m ~f = (); fun k -> m (fun a -> k (f a))
    let map = `Custom map
  end
  include T

  let yield e = (); fun k -> Wrap (Yield (e, k))

  let to_steps t = t (fun () -> Wrap Done)

  let run t =
    let init () = to_steps t in
    let f thunk = unwrap (thunk ()) in
    unfold_step ~init ~f

end

