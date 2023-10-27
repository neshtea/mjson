(lang dune 3.11)

(name mjson)

(generate_opam_files true)

(source
 (github neshtea/mjson))

(authors "Marco Schneider")

(maintainers "Marco Schneider <marco.schneider@posteo.de>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mjson)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune yojson alcotest)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
