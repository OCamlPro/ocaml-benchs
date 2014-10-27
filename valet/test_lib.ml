open Valet

module SMap = Map.Make(String)
module UuidMap = Map.Make(Uuidm)
module UuidSet = Set.Make(Uuidm)

let random_string len =
  let true_len = len / 8 * 8 + 8 in
  let b = Bytes.create true_len in
  for i = 0 to true_len / 8 - 1 do
    EndianBytes.BigEndian.set_int64 b (i*8) @@
    Random.int64 Int64.max_int
  done;
  Bytes.(sub b 0 len |> unsafe_to_string)

let gen_db n =
  let user_to_qr = Array.make n "" in
  let rec generate a = function
    | 0 -> a
    | n ->
      let qrcode = random_string 10 in
      user_to_qr.(n-1) <- qrcode;
      generate
        (SMap.add qrcode (n-1) a) @@ pred n
  in
  user_to_qr, generate SMap.empty n

module QRReader : sig
  type t

  val create : unit -> t
  val id : t -> Uuidm.t
  val handler : t -> ([`QRCode], [`QRCode]) Handler.t
  val use : t -> string -> unit
end = struct
  type t =
    { id: Uuidm.t;
      handler: ([`QRCode], [`QRCode]) Handler.t;
    }

  let create () =
      {
        id = Uuidm.create `V4;
        handler = Handler.create_sensor ();
      }

  let id t = t.id
  let handler t = t.handler

  let use t qrcode =
    let qrcode = QRCode.create ~source:t.id ~value:qrcode in
    Handler.emit t.handler @@ Event.qrcode qrcode
end

module Controller : sig
  type t

  val create : int SMap.t -> t
  val handler : t -> ([`QRCode], [`User]) Handler.t
  val connect_readers : t -> QRReader.t list -> t
end = struct
  type t =
    {
      id: Uuidm.t;
      handler: ([`QRCode], [`User]) Handler.t;
    }

  let create db =
    {
      id = Uuidm.create `V4;
      handler = Handler.create
          (fun evt -> match Event.event evt with
             | Event.QRCode qr ->
               let source = QRCode.source qr in
               let value = QRCode.value qr in
               (try Event.user @@
                  User.granted
                    ~id:(string_of_int @@ SMap.find value db)
                    ~source
                with Not_found -> Event.user `Unknown)
             | _ -> assert false (* Cannot happen because guaranteed by typing. *)
          );
    }

  let handler t = t.handler

  let connect_readers t rs =
    { t with handler = Handler.connect t.handler @@
               List.map QRReader.handler rs }
end

module Door : sig
  type t

  val create :
    readers:UuidSet.t ->
    action:(Uuidm.t -> string -> unit) -> t
  val connect_controller : t -> Controller.t -> t
end = struct
  type t =
    {
      id: Uuidm.t;
      readers: UuidSet.t;
      handler: ([`User], [`Nil]) Handler.t;
    }

  let create ~readers ~action =
    let id = Uuidm.create `V4 in
      {
        id;
        readers;
        handler = Handler.create
            (fun evt ->
               match Event.event evt with
               | Event.User v ->
                 let open User in
                 (match v with
                  | `Granted { id; source; } ->
                    if UuidSet.mem source readers then action source id;
                  | `Denied _ -> ()
                  | `Unknown -> ()
                 );
                 Event.nil ()
               | _ -> assert false
            );
      }

  let connect_controller t ctrl =
    { t with handler = Handler.connect_one t.handler @@ Controller.handler ctrl }
end
