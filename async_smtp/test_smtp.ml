open Core.Std
open Async.Std
open Async_extra.Std
open Email_message
open Async_smtp

let random_email () =
  let b = Bytes.create 25 in
  for i = 0 to 9 do
    Bytes.set b i @@ Char.of_int_exn @@ Random.int 25 + 97
  done;
  Bytes.set b 10 '@';
  for i = 11 to 20 do
    Bytes.set b i @@ Char.of_int_exn @@ Random.int 25 + 97
  done;
  Bytes.blit_string ".com" 0 b 21 4;
  b

let msgs = String.Table.create ~size:13 ()

let main n =
  let send_n_mails server n =
    let rec inner r w = function
      | n when n <= 0 -> Deferred.unit
      | n ->
        Smtp.Client.send_email r w
          ~from:(random_email ())
          ~to_:[random_email (); random_email (); random_email ()]
          Email.(empty () |> to_string) >>= function _ ->
          inner r w (pred n)
    in
    Tcp.with_connection
      (Tcp.to_host_and_port "localhost" (Tcp.Server.listening_on server))
      (* (fun _ r w -> inner r w n) *)
      (fun _ _ _ -> Deferred.unit)
  in
  let run_server a r w =
    let rec inner () =
      Smtp.Server.start_connection a r w >>= function
      | None -> inner ()
      | Some (f, t, id, msg) ->
        (String.Table.find msgs f |> function
          | None -> ignore @@ String.Table.add msgs f [t, id, msg]
          | Some v -> ignore @@
            String.Table.add msgs
              ~key:f
              ~data:((t, id, msg)::v)); inner ()
    in inner ()
  in
  Tcp.Server.create Tcp.on_port_chosen_by_os
    (fun _ r w -> run_server "bleh" r w) >>= fun server ->
  Tcp.with_connection
    (Tcp.to_host_and_port "localhost" (Tcp.Server.listening_on server))
    (fun _ _ _ -> Deferred.unit)

  (* send_n_mails server n >>= fun () -> Deferred.unit *)
  (* Tcp.Server.close server >>= fun () -> *)
  (* Shutdown.exit 0 *)

let () =
  if Array.length Sys.argv < 2 then
    (
      Printf.printf "Usage: %s n\n" Sys.argv.(0);
      Pervasives.exit 1
    );
  don't_wait_for @@ main @@ int_of_string Sys.argv.(1);
  never_returns @@ Scheduler.go ()
