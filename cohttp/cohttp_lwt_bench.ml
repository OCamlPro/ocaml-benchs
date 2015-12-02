open Lwt
open Cohttp_lwt_unix

let main n p =
  let server () =
    let open Server in
    let callback conn req body =
      respond_string ~status:`OK ~body:"Ok!" () in
    create ~mode:(`TCP (`Port p)) (make ~callback ())
  in
  let get_n n =
    let rec inner = function
      | n when n <= 0 -> Lwt.return_unit
      | n ->
          Client.(get @@ Uri.of_string @@ "http://127.0.0.1:" ^ string_of_int p)
          >>= fun (_, body) ->
          Cohttp_lwt_body.drain_body body >>= fun () ->
          inner (pred n)
    in inner n
  in
  Lwt.async server;
  get_n n

let () =
  if Array.length Sys.argv < 2 then
    (
      Printf.printf "Usage: %s <nb_requests> [<port>]\n" Sys.argv.(0);
      Pervasives.exit 1
    );
  let port = if Array.length Sys.argv < 3
    then (Random.self_init (); Random.int 10000 + 1024)
    else int_of_string Sys.argv.(2) in
  Lwt_main.run @@ main (int_of_string Sys.argv.(1)) port

let () =
  try
    let fn = Sys.getenv "OCAML_GC_STATS" in
    let oc = open_out fn in
    Gc.print_stat oc
  with _ -> ()
