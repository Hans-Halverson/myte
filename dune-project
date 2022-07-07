(lang dune 2.4)

(name myte)

(generate_opam_files true)

(license MIT)

(authors "Hans Halverson <hansghalverson@gmail.com>")

(maintainers "Hans Halverson <hansghalverson@gmail.com>")

(homepage "https://github.com/Hans-Halverson/myte")

(bug_reports "https://github.com/Hans-Halverson/myte/issues")

(package
 (name myte)
 (synopsis "Compiler for the Myte language")
 (description "Compiler for the Myte language")
 (depends
  (ocaml (= 4.14.0))
  (ocamlformat (= 0.22.4))
  (dune (= 3.3.1))))