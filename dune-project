(lang dune 3.7)
(using menhir 2.0)

(name suz)

(generate_opam_files true)

(source
 (github jgulian/suz))

(authors "Joseph Gulian")

(maintainers "Joseph Gulian")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name suz)
 (synopsis "The Suz programming language")
 (description "A language for me to experiment with plt concepts.")
 (depends ocaml dune menhir llvm core ocamlgraph)
 (tags
  (pl types)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
