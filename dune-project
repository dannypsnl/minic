(lang dune 3.14)

(name minic)

(generate_opam_files true)

(source
 (github dannypsnl/minic))

(authors "dannypsnl")

(maintainers "dannypsnl")

(license LICENSE)

(documentation https://github.com/dannypsnl/minic)

(using menhir 3.0)

(package
 (name minic)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  core
  (menhir
   (>= 20211230))
  (alcotest :with-test)
  asai
  eio
  eio_main
  ppx_deriving
  ppx_inline_test
  ppx_expect)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
