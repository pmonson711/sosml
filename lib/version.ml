type version = float

type t = { version : version }

let current =
  let open Sexplib.Sexp in
  List [ Atom "lang"; Atom "sosml"; Atom "0.1" ]


let rewrite sexp =
  let open Sexplib in
  let path = Path.parse ".[0]" in
  let head = Path.get ~path sexp in
  if head = current
  then
    Path.replace ~path sexp ~subst:Sexp.(List [ Atom "sosml"; Atom "0.1" ])
  else sexp


let t_of_sexp (sexp : Sexplib0.Sexp.t) =
  let open Sexplib in
  let path = Path.parse ".lang[1]" in
  let version =
    match Path.get ~path sexp with
    | Atom v ->
        float_of_string v
    | _      ->
        failwith
          (Printf.sprintf
             "unexpected input %s"
             (Sexplib.Sexp.to_string_hum sexp))
  in
  { version }


let sexp_of_t (v : t) =
  Sexplib.Sexp.(
    List [ Atom "lang"; Atom "sosml"; Atom (string_of_float v.version) ])


let%test _ =
  let sexp = Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "0.1" ]) in
  let v = { version = 0.1 } in
  v = t_of_sexp sexp

let%test _ =
  let version = { version = 1.0 } |> sexp_of_t in
  version = Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "1." ])

let%expect_test _ =
  Sexplib.Sexp.(List [ Atom "lang"; Atom "sosml"; Atom "1.0" ])
  |> t_of_sexp
  |> sexp_of_t
  |> Sexplib.Sexp.to_string_mach
  |> print_endline ;
  [%expect {| (lang sosml 1.) |}]
