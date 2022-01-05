open Alcotest

let sexp =
  let open Sexplib in
  testable Sexp.pp_hum Sexp.equal

let service =
  let open Sosml in
  testable Service.pp Service.equal

let empty () =
  Alcotest.(check sexp)
    "always prints name and description"
    Sexplib.Sexp.(
      List [List [Atom "description"; Atom ""]; List [Atom "name"; Atom ""]])
    Sosml.Service.({description= ""; name= ""} |> sexp_of_t)

let round_trip () =
  let subject =
    Sexplib.Sexp.(
      List
        [ List [Atom "description"; Atom "some description"]
        ; List [Atom "name"; Atom "some name"] ])
  in
  Alcotest.(check sexp)
    "round trip" subject
    Sosml.Service.(subject |> t_of_sexp |> sexp_of_t)

let case =
  ( "Service"
  , [ test_case "round trip" `Quick round_trip
    ; test_case "empty always prints" `Quick empty ] )
