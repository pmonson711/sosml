let version =
  let open Sosml in
  let open Alcotest in
  testable Version.pp Version.equal


let sexp =
  let open Sexplib in
  let open Alcotest in
  testable Sexp.pp_hum Sexp.equal


let current () =
  let open Sosml.Version in
  Alcotest.(check version)
    "is current version"
    { version = 0.1 }
    (current |> t_of_sexp)


let sexp_of_t () =
  let open Sosml.Version in
  Alcotest.(check sexp)
    "has sexp representation"
    Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "0.1" ])
    current


let round_trip () =
  Alcotest.(check sexp)
    "round trip"
    Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "1." ])
    Sosml.Version.(
      Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "1." ])
      |> t_of_sexp
      |> sexp_of_t)


let test_name = "Version"

let case =
  let open Alcotest in
  ( test_name
  , [ test_case "current is 0.1" `Quick current
    ; test_case "expected sexp representation" `Quick sexp_of_t
    ; test_case "round trip as expected" `Quick round_trip
    ] )
