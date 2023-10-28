(lang dune 3.11)

(name mjson)

(generate_opam_files true)

(source
 (github neshtea/mjson))

(authors "Marco Schneider <marco.schneider@posteo.de")

(maintainers "Marco Schneider <marco.schneider@posteo.de>")

(license BSD-3-Clause)

(package
 (name mjson)
 (synopsis "Composable, applicative and monadic DSL for decoding Yojson values")
 (description "Composable, applicative and monadic DSL for decoding Yojson values")
 (depends ocaml 
          dune 
          yojson 
          alcotest 
          ppx_deriving))
