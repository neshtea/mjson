module type S = sig
  type elt [@@deriving show, eq]

  module Error : sig
    type t =
      | Field of string * t
      | Index of int * t
      | One_of of t list
      | Failure of string * elt
    [@@deriving show, eq]

    val field : string -> t -> t
    val index : int -> t -> t
    val one_of : t list -> t
    val failure : string -> elt -> t
  end

  type 'a decoder = elt -> ('a, Error.t) result

  val string : string decoder
  val int : int decoder
  val float : float decoder
  val boolean : bool decoder
  val exactly : 'a decoder -> 'a -> 'a decoder
  val list : 'a decoder -> 'a list decoder
  val field : string -> 'a decoder -> 'a decoder
  val index : int -> 'a decoder -> 'a decoder
  val one_of : 'a decoder list -> 'a decoder
  val succeed : 'a -> 'a decoder
  val fail : string -> 'a decoder
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  val optional : 'a decoder -> 'a option decoder
  val pure : 'a -> 'a decoder
  val ap : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  val return : 'a -> 'a decoder
  val bind : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
  val map2 : ('a -> 'b -> 'c) -> 'a decoder -> 'b decoder -> 'c decoder

  val map3
    :  ('a -> 'b -> 'c -> 'd)
    -> 'a decoder
    -> 'b decoder
    -> 'c decoder
    -> 'd decoder

  module Infix : sig
    val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
    val ( >> ) : 'a decoder -> 'b decoder -> 'b decoder
    val ( <$> ) : ('a -> 'b) -> 'a decoder -> 'b decoder
    val ( <*> ) : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  end

  module Syntax : sig
    val ( let* ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder
    val ( let+ ) : 'a decoder -> ('a -> 'b) -> 'b decoder
    val ( and+ ) : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  end
end

module Make (Y : Json_impl.S) : S with type elt = Y.t = struct
  type elt = Y.t [@@deriving show, eq]

  module Error = struct
    type t =
      | Field of string * t
      | Index of int * t
      | One_of of t list
      | Failure of string * elt
    [@@deriving show, eq]

    let field field_name inner_error = Field (field_name, inner_error)
    let index idx inner_error = Index (idx, inner_error)
    let one_of inner_errors = One_of inner_errors
    let failure msg json = Failure (msg, json)
  end

  type 'a decoder = elt -> ('a, Error.t) result

  let string json =
    match Y.match_string json with
    | Some s -> Ok s
    | None -> Error (Error.failure "not a string" json)
  ;;

  let int json =
    match Y.match_int json with
    | Some i -> Ok i
    | None -> Error (Error.failure "not an int" json)
  ;;

  let float json =
    match Y.match_float json with
    | Some n -> Ok n
    | None -> Error (Error.failure "not a float" json)
  ;;

  let boolean json =
    match Y.match_bool json with
    | Some b -> Ok b
    | None -> Error (Error.failure "not a bool" json)
  ;;

  let exactly decoder expected json =
    match decoder json with
    | Ok res ->
      if res == expected
      then Ok res
      else
        Error
          (Error.failure
             (Printf.sprintf
                "not the expected value %s"
                "FIXME: generic printer")
             json)
    | Error err -> Error err
  ;;

  let list element json =
    match Y.match_list json with
    | Some arr ->
      let decoded = List.map element arr in
      if List.for_all Result.is_ok decoded
      then (
        let oks = List.map Result.get_ok decoded in
        Ok oks)
      else (
        match List.filter Result.is_error decoded with
        | err :: _ -> err |> Result.get_error |> Result.error
        | _ -> Error (Error.failure "unexpected decoder error" json))
    | None -> Error (Error.failure "not a json array" json)
  ;;

  let index idx element json =
    match Y.match_list json with
    | Some arr ->
      (match List.nth_opt arr idx with
       | Some elem ->
         (match element elem with
          | Error err -> Error (Error.index idx err)
          | Ok res -> Ok res)
       | None ->
         Error
           (Error.failure (Printf.sprintf "index out of bounds: %i" idx) json))
    | None -> Error (Error.failure "not a json array" json)
  ;;

  let one_of alternatives : 'a decoder =
    fun json ->
    let results = List.map (fun alt -> alt json) alternatives in
    let oks = List.filter Result.is_ok results in
    let errors = List.filter Result.is_error results in
    match oks with
    | [] -> Error (Error.one_of (errors |> List.map Result.get_error))
    | x :: _ -> x
  ;;

  let succeed value _json = Ok value
  let fail msg json = Error (Error.failure msg json)

  (* functor *)
  let map f decoder json = decoder json |> Result.map f
  let optional decoder = one_of [ map Option.some decoder; succeed None ]

  let field name decoder json =
    match Y.match_field name json with
    | Some value ->
      (match decoder value with
       | Ok res -> Ok res
       | Error err -> Error (Error.field name err))
    | None ->
      Error
        (Error.field
           name
           (Error.failure "not an object or field not found" json))
  ;;

  (* applicative *)
  let pure = succeed

  let ap ff fa json =
    (* <*> *)
    match fa json with
    | Ok fav ->
      (match ff json with
       | Ok ffv -> Ok (ffv fav)
       | Error error -> Error error)
    | Error error -> Error error
  ;;

  let map2 f da db =
    let ( <*> ) = ap in
    let ( <$> ) = map in
    f <$> da <*> db
  ;;

  let map3 f da db dc =
    let ( <*> ) = ap in
    let ( <$> ) = map in
    f <$> da <*> db <*> dc
  ;;

  (* monad *)
  let return = pure

  let bind decoder f json =
    Result.bind (decoder json) (fun value_a ->
      let ff = f value_a in
      ff json)
  ;;

  module Infix = struct
    let ( >>= ) = bind
    let ( >> ) da db = da >>= fun _ -> db
    let ( <*> ) = ap
    let ( <$> ) = map
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) d f = map f d

    let ( and+ ) da db =
      let tup a b = a, b in
      let open Infix in
      tup <$> da <*> db
    ;;
  end
end

module Yojson = struct
  module Safe = Make (Json_impl.Yojson_safe)
  module Basic = Make (Json_impl.Yojson_basic)
end
