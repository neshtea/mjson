let e = epsilon_float

module Safe = struct
  open Mjson.Decoder.Yojson

  let error_testable = Alcotest.(testable Safe.Error.pp Safe.Error.equal)

  let string_ok () =
    Alcotest.(check string)
      "decodes string"
      "foobar"
      (Safe.string (`String "foobar") |> Result.get_ok)
  ;;

  let string_error () =
    Alcotest.(check error_testable)
      "raises an error"
      (Safe.Error.failure "not a string" `Null)
      (Safe.string `Null |> Result.get_error)
  ;;

  let int_ok () =
    Alcotest.(check int)
      "decodes int"
      33
      (Safe.int (`Int 33) |> Result.get_ok)
  ;;

  let int_error () =
    Alcotest.(check error_testable)
      "raises an error"
      (Safe.Error.failure "not an int" `Null)
      (Safe.int `Null |> Result.get_error)
  ;;

  let float_ok () =
    Alcotest.(check @@ float @@ e)
      "decodes float"
      33.3
      (Safe.float (`Float 33.3) |> Result.get_ok)
  ;;

  let float_error () =
    Alcotest.(check error_testable)
      "raises an error"
      (Safe.Error.failure "not a float" `Null)
      (Safe.float `Null |> Result.get_error)
  ;;

  let boolean_ok () =
    Alcotest.(check bool)
      "decodes boolean"
      true
      (Safe.boolean (`Bool true) |> Result.get_ok)
  ;;

  let boolean_error () =
    Alcotest.(check error_testable)
      "raises an error"
      (Safe.Error.failure "not a bool" `Null)
      (Safe.boolean `Null |> Result.get_error)
  ;;

  let list_ok () =
    Alcotest.(check (list int))
      "decodes list of int"
      [ 1; 2; 3 ]
      (Safe.list Safe.int (`List [ `Int 1; `Int 2; `Int 3 ])
       |> Result.get_ok)
  ;;

  let list_error_1 () =
    Alcotest.(check error_testable)
      "raises error if it is not a list"
      (Safe.Error.failure "not a json array" `Null)
      (Safe.list Safe.int `Null |> Result.get_error)
  ;;

  let list_error_2 () =
    Alcotest.(check error_testable)
      "raises error if decoder does not match"
      (Safe.Error.failure "not an int" (`String "s"))
      (Safe.list Safe.int (`List [ `String "s" ]) |> Result.get_error)
  ;;

  let index_ok () =
    Alcotest.(check int)
      "decodes at specific index"
      42
      (Safe.index 1 Safe.int (`List [ `String "s"; `Int 42 ])
       |> Result.get_ok)
  ;;

  let index_error_1 () =
    Alcotest.(check error_testable)
      "raises error when out of bounds"
      (Safe.Error.failure "index out of bounds: 1" (`List [ `String "s" ]))
      (Safe.index 1 Safe.int (`List [ `String "s" ]) |> Result.get_error)
  ;;

  let index_error_2 () =
    Alcotest.(check error_testable)
      "raises error when decoder at index fails to match"
      (Safe.Error.index 0 (Safe.Error.failure "not an int" (`String "s")))
      (Safe.index 0 Safe.int (`List [ `String "s" ]) |> Result.get_error)
  ;;

  let one_of_ok_1 () =
    Alcotest.(check int)
      "decodes first match"
      42
      (Safe.one_of
         [ Safe.exactly Safe.int 42; Safe.exactly Safe.int 23 ]
         (`Int 42)
       |> Result.get_ok)
  ;;

  let one_of_ok_2 () =
    Alcotest.(check int)
      "decodes first match"
      23
      (Safe.one_of
         [ Safe.exactly Safe.int 42; Safe.exactly Safe.int 23 ]
         (`Int 23)
       |> Result.get_ok)
  ;;

  let one_of_error () =
    Alcotest.(check error_testable)
      "raises error when no decoder matches"
      (Safe.Error.one_of
         [ Safe.Error.failure
             "not the expected value FIXME: generic printer"
             (`Int 20)
         ; Safe.Error.failure
             "not the expected value FIXME: generic printer"
             (`Int 20)
         ])
      (Safe.one_of
         [ Safe.exactly Safe.int 42; Safe.exactly Safe.int 23 ]
         (`Int 20)
       |> Result.get_error)
  ;;

  let map_ok () =
    Alcotest.(check int)
      "decodes first match"
      4
      (Safe.map (fun x -> x * 2) Safe.int (`Int 2) |> Result.get_ok)
  ;;

  let map_error () =
    Alcotest.(check error_testable)
      "keeps the error"
      (Safe.Error.failure "not an int" (`String "s"))
      (Safe.map (fun x -> x * 2) Safe.int (`String "s") |> Result.get_error)
  ;;

  let optional_some () =
    Alcotest.(check @@ option @@ int)
      "some value if ok"
      (Some 42)
      (Safe.optional Safe.int (`Int 42) |> Result.get_ok)
  ;;

  let optional_none () =
    Alcotest.(check @@ option @@ int)
      "none if error"
      None
      (Safe.optional Safe.int (`String "s") |> Result.get_ok)
  ;;

  let field_ok () =
    Alcotest.(check int)
      "decodes the field"
      42
      (Safe.field "f" Safe.int (`Assoc [ "f", `Int 42 ]) |> Result.get_ok)
  ;;

  let field_error_1 () =
    Alcotest.(check error_testable)
      "error if the object does not contain the field"
      (Safe.Error.field
         "f"
         (Safe.Error.failure
            "not an object or field not found"
            (`Assoc [ "g", `Float 42.0 ])))
      (Safe.field "f" Safe.int (`Assoc [ "g", `Float 42.0 ]) |> Result.get_error)
  ;;

  let field_error_2 () =
    Alcotest.(check error_testable)
      "error if the decoder does not match the field's value"
      (Safe.Error.field "f" (Safe.Error.failure "not an int" (`String "s")))
      (Safe.field "f" Safe.int (`Assoc [ "f", `String "s" ]) |> Result.get_error)
  ;;

  (* applicative/monadic decoders *)
  type author =
    { name : string
    ; age : int
    ; has_tenure : bool
    }
  [@@deriving show, eq]

  let author_testable = Alcotest.testable pp_author equal_author
  let make_author name age has_tenure = { name; age; has_tenure }

  let author_decoder_applicative =
    let open Safe in
    let open Safe.Syntax in
    let+ name = field "name" string
    and+ age = field "age" int
    and+ has_tenure = field "tenured" boolean in
    make_author name age has_tenure
  ;;

  let ursl = make_author "Ursula" 82 true

  let author_yojson =
    `Assoc
      [ "name", `String "Ursula"; "age", `Int 82; "tenured", `Bool true ]
  ;;

  let author_decoder_applicative_ok () =
    Alcotest.(check author_testable)
      "decodes the author"
      ursl
      (author_decoder_applicative author_yojson |> Result.get_ok)
  ;;

  let author_decoder_monadic =
    let open Safe in
    let open Safe.Syntax in
    let* name = field "name" string in
    let* age = field "age" int in
    let* has_tenure = field "tenured" boolean in
    return @@ make_author name age has_tenure
  ;;

  let author_decoder_monadic_ok () =
    Alcotest.(check author_testable)
      "decodes the author"
      ursl
      (author_decoder_monadic author_yojson |> Result.get_ok)
  ;;

  type paper =
    { title : string
    ; authors : author list
    }
  [@@deriving show, eq]

  let paper_testable = Alcotest.testable pp_paper equal_paper
  let make_paper title authors = { title; authors }
  let isaac = make_author "Isaac" 73 false
  let paper = make_paper "How to Write an okay JSON Decoder" [ ursl; isaac ]

  let paper_yojson =
    `Assoc
      [ "title", `String "How to Write an okay JSON Decoder"
      ; ( "authors"
        , `List
            [ author_yojson
            ; `Assoc
                [ "name", `String "Isaac"
                ; "age", `Int 73
                ; "tenured", `Bool false
                ]
            ] )
      ]
  ;;

  let paper_decoder_monadic =
    let open Safe in
    let open Safe.Syntax in
    let* title = field "title" string in
    let* authors = field "authors" (list author_decoder_monadic) in
    return @@ make_paper title authors
  ;;

  let paper_decoder_monadic_ok () =
    Alcotest.(check paper_testable)
      "decodes the paper and all authors"
      paper
      (paper_decoder_monadic paper_yojson |> Result.get_ok)
  ;;

  let suite =
    [ ( "basic decoder"
      , [ Alcotest.test_case "string ok" `Quick string_ok
        ; Alcotest.test_case "string error" `Quick string_error
        ; Alcotest.test_case "int ok" `Quick int_ok
        ; Alcotest.test_case "int error" `Quick int_error
        ; Alcotest.test_case "float ok" `Quick float_ok
        ; Alcotest.test_case "float error" `Quick float_error
        ; Alcotest.test_case "boolean ok" `Quick boolean_ok
        ; Alcotest.test_case "boolean error" `Quick boolean_error
        ; Alcotest.test_case "list ok" `Quick list_ok
        ; Alcotest.test_case "list error (not a list)" `Quick list_error_1
        ; Alcotest.test_case
            "list error (decoder doesn't match)"
            `Quick
            list_error_2
        ] )
    ; ( "composed decoder"
      , [ Alcotest.test_case "index ok" `Quick index_ok
        ; Alcotest.test_case "index error (out of bounds)" `Quick index_error_1
        ; Alcotest.test_case
            "index error (decoder does not match)"
            `Quick
            index_error_2
        ; Alcotest.test_case "one_of ok" `Quick one_of_ok_1
        ; Alcotest.test_case "one_of ok" `Quick one_of_ok_2
        ; Alcotest.test_case "one_of error" `Quick one_of_error
        ; Alcotest.test_case "optional some" `Quick optional_some
        ; Alcotest.test_case "optional none" `Quick optional_none
        ; Alcotest.test_case "field ok" `Quick field_ok
        ; Alcotest.test_case
            "field error (field not found)"
            `Quick
            field_error_1
        ; Alcotest.test_case
            "field error (decoder does not match field)"
            `Quick
            field_error_2
        ] )
    ; ( "decoder functor"
      , [ Alcotest.test_case "map ok" `Quick map_ok
        ; Alcotest.test_case "map error" `Quick map_error
        ] )
    ; ( "decoder applicative"
      , [ Alcotest.test_case
            "applicative decoder"
            `Quick
            author_decoder_applicative_ok
        ] )
    ; ( "decoder monadic"
      , [ Alcotest.test_case
            "monadic flat decoder"
            `Quick
            author_decoder_monadic_ok
        ; Alcotest.test_case
            "monadic nested objects/lists decoder"
            `Quick
            paper_decoder_monadic_ok
        ] )
    ]
  ;;
end

let () =
  let open Alcotest in
  run "Mjson" Safe.suite
;;
