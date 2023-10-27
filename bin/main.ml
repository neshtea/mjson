let () =
  match Mjson.Decoder.Yojson.Safe.string (`String "mjson") with
  | Ok _ -> print_endline "ok"
  | Error _ -> print_endline "oops"
;;

