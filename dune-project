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
    (parsexp (>= 0.13))

    (merlin :dev)
    (ocamlformat :dev)
    (ocp-indent :dev)
    (ocp-index :dev)
    (odig :dev)
    (utop :dev)
  )
 )
