`mjson` is a small OCaml library for decoding JSON values from more low-level 
JSON libraries such as [Yojson](https://github.com/ocaml-community/yojson).

It is just a thin layer on top of Yojson (`Safe` and `Basic`).

# Examples

```ocaml
type person =
  { name : string
  ; age : int
  }

let make_person name age = { name; age }

(* (monadic) decoder for Yojson.Safe.t values into a person. *)
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

(* (applicative) decoder for Yojson.Basic.t values into a person. *)
let person_decoder_applicative =
  let open Mjson.Decoder.Basic in
  let open Mjson.Decoder.Basic.Syntax in
  let+ name = field "name" string
  and+ age = field "name" int in
  make_person name age

let () =
  let yojson : Yojson.Safe.t = `Assoc [ "name", `String "Ursula"; "age": `Float 88.0] in
  match person_decoder yojson with
    | Ok person -> Some person
    | Error err ->
        Printf.printf "Error parsing json: %s" (err |> Mjson.Decoder.Safe.Error.show);
        None
```

See the [tests](./test/decoder.ml) for more examples.

# Building the Code

This packages comes with a nix DevShell.  All commands are intended for use 
within the provided (default) devShell.

```shell
$ make build  # build the library
$ make repl   # opens a utop repl with the project loaded
$ make test   # runs all tests
$ make doc    # generate all documentation
```
