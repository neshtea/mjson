(** {1 The Basics}

    The decoder library is a generic applicative and monadic DSL for decoding
    (parsed) JSON.  It provides implementations for {!Yojson.Safe} and
    {!Yojson.Basic} but is -- in general -- unopinionated to the lower level
    JSON parser.

    A ['a decoder] is a function from a lower-level JSON representation to an
    ['a] or an {!S.Error.t}.

    The underlying monad allows us to not care about errors when building up
    decoders.  Each decoder might evaluate to a value of the type it encodes or
    an error.  Monadically binding the results makes us to not care about
    errors in the decoders

    {2 Decoders}

    The {!Decoder} module provides simple building blocks to build up JSON
    decoders.  There are basic building blocks for decoding JSON primitives:

    - {!S.string}
    - {!S.float}
    - {!S.boolean}
    - {!S.list} treated as homogenous lists, see {!S.index} for conceptually
      heterogenous access to list
    - {!S.field} which provides field-based access to objects

    There are also decoders for more Ocaml specific types, such as {!S.int}.
    See the interface documentation for an overview.

    {2 Example Usage}

    {[
      type person =
        { name : string
        ; age : int
        }

      let make_person name age = { name; age }

      let person_decoder =
        let open Mjson.Decoder.Safe in
        let open Mjson.Decoder.Safe.Syntax in
        (* decode the field "name" into a string *)
        let* name = field "name" string in
        (* decode the field "age" into an int *)
        let* age = field "age" int in
        (* if all went well [return] the person (or any error that might have
           occured along the way. *)
        return @@ make_person name age
      ;;

      let () =
        let yojson : Yojson.Safe.t = `Assoc [ "name", `String "Ursula"; "age": `Float 88.0] in
        match person_decoder yojson with
        | Ok person -> Some person
        | Error err ->
            Printf.printf "Error parsing json: %s" (err |> Mjson.Decoder.Safe.Error.show);
            None
    ]} *)

(** Main interface of the Decoder. *)
module type S = sig
  type elt [@@deriving show, eq]

  (** Errors specific to the Decoder monad. *)
  module Error : sig
    (** Only for documentation purposes and pattern matching.  Please use the
        constructors below to create errors. *)
    type t =
      | Field of string * t
      | Index of int * t
      | One_of of t list
      | Failure of string * elt
    [@@deriving show, eq]

    (** {1 Constructors} *)

    (** [field field_name inner_error] is an error that occurs when unable to
        decode the underlying value of a field in an object *)
    val field : string -> t -> t

    (** [index idx inner_error] is an error that occurs then unable to decode
        the value at a specific index in a list. *)
    val index : int -> t -> t

    (** [one_of inner_errors] is an error that occurs when unable to decode a
        value from a {!one_of}. *)
    val one_of : t list -> t

    (** [failure msg json] is a generic, base-failure for decoders that
        contains a human readable error message and the piece of JSON that
        could not be decoded. *)
    val failure : string -> elt -> t
  end

  (** The decoder is a function that maps some JSON implementation's value to a
      decoded value or an error. *)
  type 'a decoder = elt -> ('a, Error.t) result

  (** When we talk about something 'decoding' a value, we mean an ['a decoder].

      {1 Decoders}

      {2 Basic Decoders} *)

  (** [string] is a decoder that decodes a JSON value to a OCaml string. *)
  val string : string decoder

  (** [int] is a decoder that decodes a JSON double to an OCaml int. *)
  val int : int decoder

  (** [float] is a decoder that decodes a JSON double to an OCaml float. *)
  val float : float decoder

  (** [boolean] is a decoder that decodes a JSON boolean to an OCaml bool. *)
  val boolean : bool decoder

  (** [exactly decoder value] is a decoder that decodes a JSON string using
      [decoder] and compares it's result to [value].  If it matches that
      value exactly, the decode is successful. *)
  val exactly : 'a decoder -> 'a -> 'a decoder

  (** [list decoder] is a decoder that decodes a JSON array into an OCaml list
      using [decoder] for every element in the array. *)
  val list : 'a decoder -> 'a list decoder

  (** [field field_name decoder] is a decoder that decodes a field of a JSON
      object into an ocaml value, using [decoder] on the value of the field. *)
  val field : string -> 'a decoder -> 'a decoder

  (** [index idx decoder] is a decoder that decodes an element of a JSON array
      at index [idx], using the [decoder]. *)
  val index : int -> 'a decoder -> 'a decoder

  (** [one_of decoders] is a decoder that decodes a JSON value using one of the
      [decoders] (first match wins). *)
  val one_of : 'a decoder list -> 'a decoder

  (** [succeed value] is a decoder that always matches and returns the [value]. *)
  val succeed : 'a -> 'a decoder

  (** [fail message] is a decoder that always fails with [message] as the
      failure message. *)
  val fail : string -> 'a decoder

  (** {2 Composing Decoders}

      We can make up new decoders be composing decoders together.  As it turns
      out, {!decoder}s are functors, applicatives and monads, which makes it
      very nice and easy to compose them.

      {3 Functor} *)

  (** [map f decoder] applies [f] to the result of [decoder], returning a new
      decoder. *)
  val map : ('a -> 'b) -> 'a decoder -> 'b decoder

  (** [optional decoder] is a decoder instead of returning an error on failure
      returns None or Some. *)
  val optional : 'a decoder -> 'a option decoder

  (** {3 Applicative} *)

  (** [pure value] lifts a value into the decoder applicative. *)
  val pure : 'a -> 'a decoder

  (** [ap d e] applies the function in decoder [d] onto the value wrapped in
      decoder [e], returning a new decoder. *)
  val ap : ('a -> 'b) decoder -> 'a decoder -> 'b decoder

  (** {3 Monad} *)

  (** [return value] lifts a value into the decoder monad. *)
  val return : 'a -> 'a decoder

  (** [bind m mf] sequentially applies decoders [m] and then [mf]. *)
  val bind : 'a decoder -> ('a -> 'b decoder) -> 'b decoder

  val map2 : ('a -> 'b -> 'c) -> 'a decoder -> 'b decoder -> 'c decoder

  val map3
    :  ('a -> 'b -> 'c -> 'd)
    -> 'a decoder
    -> 'b decoder
    -> 'c decoder
    -> 'd decoder

  (** Infix operators for applicative and monadic functions. *)
  module Infix : sig
    (** See {!bind} *)
    val ( >>= ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder

    (** Monadic bind that 'forgets' about the result of the first decoder.
        Useful if you want to make sure certain value exists and can be
        decoded but are not interested in it's actual value. *)
    val ( >> ) : 'a decoder -> 'b decoder -> 'b decoder

    (** See {!map} *)
    val ( <$> ) : ('a -> 'b) -> 'a decoder -> 'b decoder

    (** See {!ap} *)
    val ( <*> ) : ('a -> 'b) decoder -> 'a decoder -> 'b decoder
  end

  (** Syntax for applicative and monadic functions. *)
  module Syntax : sig
    (** See {!bind} *)
    val ( let* ) : 'a decoder -> ('a -> 'b decoder) -> 'b decoder

    (** Flipped version of {!map}. *)
    val ( let+ ) : 'a decoder -> ('a -> 'b) -> 'b decoder

    (** Fanout the result of two sequential decoders into a decoder of a tuple. *)
    val ( and+ ) : 'a decoder -> 'b decoder -> ('a * 'b) decoder
  end
end

(** {2 Implementations} *)

(** Implementations of the decoder monad for Yojson. *)
module Yojson : sig
  (** Implementation of the decoder monad for values of type
      [Yojson.Safe.t]. *)
  module Safe : S with type elt = Json_impl.Yojson_safe.t

  (** Implementation of the decoder monad for values of type
      [Yojson.Basic.t]. *)
  module Basic : S with type elt = Json_impl.Yojson_basic.t
end
