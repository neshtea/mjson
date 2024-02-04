`mjson` is a small OCaml library for decoding JSON values from more low-level 
JSON libraries such as [Yojson](https://github.com/ocaml-community/yojson).

The current implementation comes with a thin layer on top of Yojson (`Safe` and
`Basic`).

# Examples

```ocaml
type person =
  { name : string
  ; age : int
  }

let make_person name age = { name; age }

(* (monadic) decoder for Yojson.Safe.t values into a person. *)
let person_decoder =
  let module D = Mjson.Decoder.Yojson.Safe in
  let ( let* ) = Mjson.Decoder.Yojson.Safe.bind in
  (* decode the field "name" into a string *)
  let* name = D.field "name" string in
  (* decode the field "age" into an int *)
  let* age = D.field "age" int in
  (* if all went well [return] the person (or any error that might have
     occured along the way. *)
  D.return @@ make_person name age
  ;;
```

Example utop session

```ocaml
utop # person_decoder @@ `Assoc [ "name", `String "Ursula"; "age", `Int 88];;
- : (person, Mjson.Decoder.Yojson.Safe.Error.t) result =
Ok {name = "Ursula"; age = 88}

utop # person_decoder @@ `Assoc [ "name", `String "Ursula"; "age", `Bool true];;
- : (person, Mjson.Decoder.Yojson.Safe.Error.t) result =
Error
 (Mjson.Decoder.Yojson.Safe.Error.Field ("age",
   Mjson.Decoder.Yojson.Safe.Error.Failure ("not an int", `Bool true)))


```

See the [tests](./test/decoder.ml) for more examples.

# Building the Code

This packages comes with a nix DevShell.  All commands are intended for use 
within the provided (default) devShell.

```shell
$ make repl   # opens a utop repl with the project loaded
$ make test   # runs all tests
$ make doc    # generate all documentation
```
