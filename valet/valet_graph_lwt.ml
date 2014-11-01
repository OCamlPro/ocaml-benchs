open Valet_graph
open Lwt

let random_string len =
  let true_len = len / 8 * 8 + 8 in
  let b = Bytes.create true_len in
  for i = 0 to true_len / 8 - 1 do
    EndianBytes.BigEndian.set_int64 b (i*8) @@
    Random.int64 Int64.max_int
  done;
  Bytes.(sub b 0 len |> unsafe_to_string)

let list_init n f =
  let rec inner acc = function
    | n when n < 1 -> acc
    | n ->
    let v = f () in inner (v::acc) (pred n)
  in inner [] n

let main n =
  let controllers = Array.init n (fun _ ->
      Vertex.create Event.Key Event.Access
        (fun id st key ->
           let st, acs = State.check_key st id key in
           if acs <> [] then st, `Granted
           else st, `Denied
        )
    )
  in
  let actuators = Array.init n (fun _ ->
      Vertex.create Event.Access Event.DAGEnd
        (fun id st -> function
           | `Granted ->
               State.append_log st "Actuator %s granted" (Uuidm.to_string id), ()
           | `Denied ->
               State.append_log st "Actuator %s denied" (Uuidm.to_string id), ()
        )
    )
  in
  let gen_accesses () =
    let acs = ref [] in
    for i = 1 to n/2 do
      let i = Random.int n in
      acs := Vertex.(controllers.(i).id, [`Lock; `Unlock])::!acs
    done;
    !acs
  in
  let users = Array.init n (fun i ->
      User.create
        ~keys:[random_string 16]
        ~accesses:(gen_accesses ())
      @@ string_of_int i
    )
  in
  let st = Array.fold_left (fun a u -> State.add_user a u ) State.empty users in
  return_unit

let () =
  if Array.length Sys.argv < 2 then
    begin
      Printf.eprintf "Usage: %s n\n" Sys.argv.(0);
      exit 1
    end;
  Lwt_main.run @@ main @@ int_of_string Sys.argv.(1)
