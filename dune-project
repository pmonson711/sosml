(lang dune 2.5)
(name sosml)

(generate_opam_files true)
(version unreleased)

(authors "Paul Monson")
(maintainers "pmonson711@gmail.com")
(license MIT)
(source (uri "git+http://gitlab.com"))
(homepage "http://gitlab.com")
(bug_reports "http://gitlab.com")
(documentation "http://gitlab.com")

(package
  (name sosml)
  (synopsis "Command line client for evenstore")
  (description "Command line client for evenstore")
  (depends
    (dune (>= 2.5))
    (ocaml (>= 4.08))

    (bos (>= 0.2))
    (cmdliner (>= 1.0))
    (ppx_sexp_conv (>= 0.13))
    (sexplib (>= 0.13))
    (ppx_deriving (>= 4.5))

    (ppx_inline_test (and :with-test (>= 0.13)))
    (ppx_expect (and :with-test (>= 0.13)))
    (alcotest-lwt (and :with-test (>= 1.1)))

    (merlin :dev)
    (ocamlformat :dev)
    (ocp-indent :dev)
    (ocp-index :dev)
    (odig :dev)
    (utop :dev)
  )
 )
