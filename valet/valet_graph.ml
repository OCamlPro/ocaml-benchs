module UMap = Map.Make(Uuidm)
module USet = Set.Make(Uuidm)

module RString : sig
  val create : int -> bytes
end = struct
  let create len =
    let true_len = len / 8 * 8 + 8 in
    let b = Bytes.create true_len in
    for i = 0 to true_len / 8 - 1 do
      EndianBytes.BigEndian.set_int64 b (i*8) @@
      Random.int64 Int64.max_int
    done;
    Bytes.(sub b 0 len |> unsafe_to_string)
end

module Action : sig
  type t = [`Lock | `Unlock]
  val compare : t -> t -> int
end = struct
  type t = [`Lock | `Unlock]
  let compare = Pervasives.compare
end

module ActionSet = Set.Make(Action)

module Key : sig
  type t = string

  val create : string -> t
  val generate : int -> t
  val compare : t -> t -> int
end = struct
  type t = string

  let create value = value

  let generate len = RString.create len

  let compare = Pervasives.compare
end

module KeySet = Set.Make(Key)
module KeyMap = Map.Make(Key)

module User : sig
  type t = private {
    id: Uuidm.t;
    name: string;
    keys: KeySet.t;
    accesses: ActionSet.t UMap.t;
  }

  val create :
    ?keys:Key.t list ->
    ?accesses:(Uuidm.t * Action.t list) list ->
    string -> t
  val compare : t -> t -> int

  val add_key : t -> Key.t -> t
  val add_keys : t -> Key.t list -> t
  val remove_key : t -> Key.t -> t
  val remove_keys : t -> t
  val has_key : t -> Key.t -> bool

  val add_access : t -> Uuidm.t -> Action.t list -> t
  val add_accesses : t -> (Uuidm.t * Action.t list) list -> t
  val remove_access : t -> Uuidm.t -> t
  val remove_accesses : t -> t
  val access : t -> Uuidm.t -> Action.t list
end = struct
  type t = {
    id: Uuidm.t;
    name: string;
    keys: KeySet.t;
    accesses: ActionSet.t UMap.t;
  }

  let add_accesses t accesses =
    let accesses = List.map
        (fun (uuid, acs) -> (uuid, ActionSet.of_list acs)) accesses in
    { t with accesses =
               List.fold_left
                 (fun acc (uuid, aset) -> UMap.add uuid aset acc)
                 t.accesses accesses
    }

  let add_access t uuid acs = {
    t with accesses = UMap.add uuid (ActionSet.of_list acs) t.accesses
  }

  let remove_access t uuid = {
    t with accesses = UMap.remove uuid t.accesses
  }

  let remove_accesses t = {
    t with accesses = UMap.empty
  }

  let access t uuid =
    try UMap.find uuid t.accesses |> ActionSet.elements
    with Not_found -> []

  let add_keys t keys = {
    t with keys = KeySet.union t.keys @@ KeySet.of_list keys
  }

  let add_key t key = {
    t with keys = KeySet.add key t.keys
  }

  let remove_key t key = {
    t with keys = KeySet.remove key t.keys
  }

  let remove_keys t = {
    t with keys = KeySet.empty
  }

  let has_key t k = KeySet.mem k t.keys

  let create ?(keys = []) ?(accesses = []) name =
    let t = {
      id = Uuidm.create `V4;
      name;
      keys = KeySet.empty;
      accesses = UMap.empty;
    } in
    add_keys (add_accesses t accesses) keys

  let compare t1 t2 = Uuidm.compare t1.id t2.id
end

module UserSet = Set.Make(User)
module UserMap = Map.Make(User)

module Event : sig
  type auth = [`Valid of Uuidm.t * Uuidm.t | `Invalid of Uuidm.t]
  type access = [`Granted | `Denied]
  type _ ty =
    | Error : Uuidm.t ty
    | DAGEnd : unit ty
    | Key : string ty
    | Auth : auth ty
    | Access : access ty
    (** Type witnesses of above types. *)

  type (_,_) eq = Eq : ('a,'a) eq

  type t = Dyn : 'a ty * 'a -> t

  val error : Uuidm.t -> t
  val dag_end : t
  val key : string -> t
  val auth_valid : Uuidm.t -> Uuidm.t -> t
  val auth_invalid : Uuidm.t -> t
  val access_granted : unit -> t
  val access_denied : unit -> t

  val eq_type : 'a ty -> 'b ty -> ('a, 'b) eq option
  val get_evt : t -> 'a ty -> 'a option

end = struct
  type auth = [`Valid of Uuidm.t * Uuidm.t | `Invalid of Uuidm.t]
  type access = [`Granted | `Denied]
  type _ ty =
    | Error : Uuidm.t ty
    | DAGEnd : unit ty
    | Key : string ty
    | Auth : auth ty
    | Access : access ty
    (** Type witnesses of above types. *)

  type (_,_) eq = Eq : ('a,'a) eq

  let eq_type : type a b. a ty -> b ty -> (a,b) eq option =
    fun ty ty' -> match ty, ty' with
      | Error, Error -> Some Eq
      | DAGEnd, DAGEnd -> Some Eq
      | Key, Key -> Some Eq
      | Auth, Auth -> Some Eq
      | Access, Access -> Some Eq
      | _ -> None

  type t = Dyn : 'a ty * 'a -> t

  let error id = Dyn(Error, id)
  let dag_end = Dyn(DAGEnd, ())
  let key s = Dyn(Key, s)
  let auth_valid id id' = Dyn(Auth, `Valid (id, id'))
  let auth_invalid id = Dyn(Auth, `Invalid id)
  let access_granted () = Dyn(Access, `Granted)
  let access_denied () = Dyn(Access, `Denied)

  let get_evt : type a. t -> a ty -> a option =
    fun (Dyn (ty, a)) ty'->
      (match eq_type ty ty' with Some Eq -> Some a | None -> None)
end

module Uuidm_with_hash = struct
  include Uuidm
  let hash t =
    let b = to_bytes t in
    let sum = ref 0 in
    String.iter (fun c -> sum := !sum + Char.code c) b;
    !sum
end

module rec Vertex : sig
  type dyn
  type t = private {
    id: Uuidm.t;
    f: dyn;
  }

  val create : 'a Event.ty -> 'b Event.ty ->
    (Uuidm.t -> State.t -> 'a -> State.t * 'b) -> t

  val app : t -> State.t -> Event.t -> (State.t * Event.t)
end = struct
  type dyn = Dyn : 'a Event.ty * 'b Event.ty *
                   (Uuidm.t -> State.t -> 'a -> State.t * 'b) -> dyn

  type t = {
    id: Uuidm.t;
    f: dyn;
  }

  let create ty ty' f = {
    id = Uuidm.create `V4;
    f = Dyn (ty, ty', f);
  }

  let app t st evt = match (t.f, evt) with
    | Dyn(ty, ty', f), Event.Dyn(ty'', e) ->
        match Event.eq_type ty ty'' with
        | None -> st, Event.error t.id
        | Some Event.Eq ->
            let st, evt = f t.id st e in
            st, Event.Dyn(ty', evt)
end

and State : sig
  type t

  val empty : t

  val add_user : t -> User.t -> t
  val add_users : t -> User.t list -> t
  val remove_user : t -> Uuidm.t -> t
  val with_user : t -> Uuidm.t -> (User.t -> User.t option) -> [ `Ok of t | `Not_found ]

  val add_vertex : t -> Vertex.t -> t
  val remove_vertex : t -> Uuidm.t -> t
  val remove_vertices : t -> t

  val add_edge : t -> Uuidm.t -> Uuidm.t -> t
  val remove_edge : t -> Uuidm.t -> Uuidm.t -> t

  val append_log : t -> ('a, unit, string, t) format4 -> 'a
  val app : t -> Uuidm.t -> Event.t -> t

  val check_key : t -> Uuidm.t -> Key.t -> t * Action.t list
end = struct
  module UDAG = struct
    include Graph.Persistent.Digraph.Concrete(Uuidm_with_hash)

    let sensors t =
      fold_vertex
        (fun v a -> if in_degree t v = 0 then USet.add v a else a)
        t USet.empty

    let actuators t =
      fold_vertex
        (fun v a -> if out_degree t v = 0 then USet.add v a else a)
        t USet.empty
  end

  type t = {
    users: User.t UMap.t;
    vertices: Vertex.t UMap.t;
    dag: UDAG.t;
    log: string list;
  }

  let empty = {
    users = UMap.empty;
    vertices = UMap.empty;
    dag = UDAG.empty;
    log = [];
  }

  let add_user t u = User.{ t with users = UMap.add u.id u t.users }
  let remove_user t id = { t with users = UMap.remove id t.users }
  let with_user t id f =
    try
      let u = UMap.find id t.users in
      match f u with
      | Some u -> `Ok (add_user t u)
      | None -> `Ok (remove_user t u.User.id)
    with Not_found -> `Not_found

  let add_users t us =
    User.{ t with users = List.fold_left (fun a u -> UMap.add u.id u a) t.users us }

  let add_vertex t v = Vertex.{ t with
                                dag = UDAG.add_vertex t.dag v.id;
                                vertices = UMap.add v.id v t.vertices;
                              }

  let remove_vertex t vid = { t with
                              dag = UDAG.remove_vertex t.dag vid;
                              vertices = UMap.remove vid t.vertices;
                            }

  let remove_vertices t = { t with
                            dag = UDAG.empty;
                            vertices = UMap.empty;
                          }

  let add_edge t vid1 vid2 = { t with
                               dag = UDAG.add_edge t.dag vid1 vid2;
                             }

  let remove_edge t vid1 vid2 = { t with
                                  dag = UDAG.remove_edge t.dag vid1 vid2;
                                }

  let append_log t =
    Printf.kprintf (fun s -> { t with log = s :: t.log })

  let app t id evt =

    (* This function fold the DAG in a manner similar to React *)
    let app' t v evt =
      UDAG.fold_succ
        (fun v (t, evt) ->
           try
             let v = UMap.find v t.vertices in
             Vertex.app v t evt
           with Not_found -> (t, evt)
        )
        t.dag id (t, evt)
    in
    (* If id is not a sensor or does not emit events of the same kind
       as event, log and do nothing *)
    try
      let sensor = UMap.find id t.vertices in
      let st, evt = app' t sensor evt in
      assert (evt = Event.dag_end);
      st
    with
      Not_found ->
        append_log t "Sensor %s not found in DB." (Uuidm.to_string id)

  let check_key t rid key =
    UMap.fold
      (fun id u a -> if User.has_key u key then Some u else a)
      t.users None
    |> function
    | Some u -> t, User.access u rid
    | None -> append_log t "No users with key %s" key, []
end

