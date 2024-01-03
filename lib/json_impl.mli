(** Basic functionality for interacting with Yojson (Safe and Basic) to base
    our implementation on. *)
module type S = sig
  type t [@@deriving show, eq]

  val match_string : t -> string option
  val match_int : t -> int option
  val match_float : t -> float option
  val match_bool : t -> bool option
  val match_list : t -> t list option
  val match_field : string -> t -> t option
end

module Yojson_safe : S with type t = Yojson.Safe.t
module Yojson_basic : S with type t = Yojson.Basic.t
