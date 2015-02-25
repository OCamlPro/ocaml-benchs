type ('s,'a) unfolder =
  {unfold :
     'r.
        's
     -> on_done:'r
     -> on_skip:('s -> 'r)
     -> on_yield:('s -> 'a -> 'r)
     -> 'r}

type _ t =
  | Sequence : ('s * ('s,'a) unfolder) -> 'a t

let map (Sequence(s,{unfold})) ~f =
  Sequence(s, {unfold =
                 fun s ~on_done ~on_skip ~on_yield ->
                   let on_yield s a = on_yield s (f a) in
                   unfold s ~on_done ~on_skip ~on_yield})

let filter (Sequence(s,{unfold})) ~f =
  Sequence(s, {unfold =
                 fun s ~on_done ~on_skip ~on_yield ->
                   let on_yield s a =
                     if f a
                     then on_yield s a
                     else on_skip s in
                   unfold s ~on_done ~on_skip ~on_yield})

let fold_1 (Sequence(s,{unfold})) ~init ~f =
  let rec loop s v =
    unfold s ~on_done:v ~on_skip:(fun s -> loop s v)
      ~on_yield:(fun s a -> loop s (f v a))
  in
  loop s init

let fold_2 (Sequence(s,{unfold})) ~init ~f =
  let s_ref = ref s in
  let v_ref = ref init in
  while begin
    unfold
      !s_ref
      ~on_done:false
      ~on_skip:(fun s -> s_ref:=s; true)
      ~on_yield:
        (fun s a ->
          s_ref := s;
          v_ref := f !v_ref a;
          true)
  end do () done;
  !v_ref


let fold = fold_2

let (|>) x f = f x


