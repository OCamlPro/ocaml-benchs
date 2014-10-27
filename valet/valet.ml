open React

(* Events *)
module QRCode = struct
  type t =
    {
      source: Uuidm.t;
      value: string;
    }
  let create ~source ~value = { source; value; }
  let check t s = t.value = s
  let compare = compare
  let source t = t.source
  let value t = t.value
end

module User = struct
  type _t =
    {
      id: string;
      source: Uuidm.t;
    }

  type t = [ `Granted of _t | `Denied of _t | `Unknown ]

  let granted ~id ~source = `Granted { id; source; }
  let denied ~id ~source = `Denied { id; source; }
end

module Event = struct
  type kind = [`QRCode | `User | `Nil]

  type event =
    | QRCode of QRCode.t
    | User of User.t
    | Nil

  type +'a t =
    {
      event: event;
      timestamp: float;
    }

  let create event =
    {
      event = event;
      timestamp = Unix.gettimeofday ();
    }


  let event t = t.event
  let qrcode qr = create @@ QRCode qr
  let user u = create @@ User u
  let nil () = create Nil
end

module Handler = struct
  type ('a, 'b) t =
    {
      evt: 'b Event.t event;
      send: ?step:step -> 'a Event.t -> unit;
      action: ('a Event.t -> 'b Event.t); (* we need it for later *)
    }

  let create action =
    let evt, send = E.create () in
    let evt = E.map action evt
    in
    {
      evt; send; action;
    }

  let create_sensor () =
    let evt, send = E.create () in
    { evt; send; action = fun e -> e; }

  let emit ?step t e = t.send ?step e

  let connect t others =
    let evts = List.map
        (fun o -> E.map t.action o.evt)
        others in
    let evt = E.select @@ t.evt :: evts in
    {
      t with evt;
    }

  let connect_one t t' = connect t [t']
end
