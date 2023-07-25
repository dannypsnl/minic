(lang dune 3.9)

(name minic)

(generate_opam_files true)

(source
 (github dannypsnl/minic))

(authors "dannypsnl")

(maintainers "dannypsnl")

(license LICENSE)

(documentation https://github.com/dannypsnl/minic)

(package
 (name minic)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  ppx_deriving
  ppx_inline_test
  ppx_expect
  parsexp
  parsexp_io)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
