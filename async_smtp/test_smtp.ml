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
    let rec inner = function
      | n when n <= 0 -> Deferred.unit
      | n ->
          Tcp.with_connection
            (Tcp.to_host_and_port "localhost" (Tcp.Server.listening_on server))
            (fun _ r w ->
               Smtp.Client.send_email r w
                 ~from:(random_email ())
                 ~to_:[random_email (); random_email (); random_email ()]
                 Email.(empty () |> to_string)
            ) >>= fun _ -> inner (pred n)
    in inner n
  in
  let run_server a r w =
      Smtp.Server.start_connection a r w >>| function
      | None -> ()
      | Some (f, t, id, msg) ->
          (String.Table.find msgs f |> function
            | None -> ignore @@ String.Table.add msgs f [t, id, msg]
            | Some v -> ignore @@
                String.Table.add msgs
                  ~key:f
                  ~data:((t, id, msg)::v))
  in
  Tcp.Server.create Tcp.on_port_chosen_by_os
    (fun _ r w -> run_server "bleh" r w) >>= fun server ->
  send_n_mails server n >>= fun () ->
  Tcp.Server.close server >>= fun () ->
  Printf.printf "%d messages in mailbox.\n" @@ String.Table.length msgs;
  Shutdown.exit 0

let () =
  if Array.length Sys.argv < 2 then
    (
      Printf.printf "Usage: %s n\n" Sys.argv.(0);
      Pervasives.exit 1
    );
  don't_wait_for @@ main @@ int_of_string Sys.argv.(1);
  never_returns @@ Scheduler.go ()
