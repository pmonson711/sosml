type t =
  [ `Parsed    of Sexplib.Sexp.t
  | `Rewritten of Sexplib.Sexp.t
  ]

let get_val = function `Parsed value -> value | `Rewritten value -> value

let parsed value = `Parsed value

let rewritten value = `Rewritten value

let wrap = Printf.sprintf "(%s)"

let parse str =
  str |> wrap |> Parsexp.Single.parse_string |> Result.map parsed

let parse_exn str = str |> wrap |> Parsexp.Single.parse_string_exn |> parsed

let print_endline sexp =
  sexp |> get_val |> Sexplib0.Sexp.to_string_hum |> print_endline

let unwrap =
  let open Sexplib in
  let unwrap = Path.parse ".[0]" in
  Path.get ~path:unwrap


let get_version (`Parsed sexp) = sexp |> unwrap |> Version.t_of_sexp

let rewrite_version (`Parsed sexp) = Version.rewrite sexp |> rewritten

let get_service (`Rewritten sexp) = sexp |> Service.t_of_sexp

let get_structures sexp =
  sexp
  |> get_val
  |> Structure.get_key "structures"
  |> Option.value ~default:[]
  |> List.map Structure.t_of_sexp
  |> List.flatten


let get_events sexp =
  sexp
  |> get_val
  |> Structure.get_key "events"
  |> Option.value ~default:[]
  |> List.map Structure.t_of_sexp
  |> List.flatten


(****************************************************************************)
(*** Expect Tests ***********************************************************)
(****************************************************************************)

let%expect_test _ =
  let src =
    {|
(lang sosml 0.1)
(description "This is just a test for the tendering concept")
(name tendering)
    |}
  in
  parse_exn src |> print_endline ;
  [%expect
    {|
    ((lang sosml 0.1)
     (description "This is just a test for the tendering concept")
     (name tendering)) |}]

let%test _ =
  let src =
    {|
(lang sosml 0.1)
(description "This is just a test for the tendering concept")
(name tendering)
    |}
  in
  let lang = parse_exn src |> get_version in
  Version.{ version = 0.1 } = lang

let%test _ =
  let src =
    {|
(lang sosml 0.1)
(description "This is just a test for the tendering concept")
(name tendering)
    |}
  in
  let service = parse_exn src |> rewrite_version |> get_service in
  Service.
    { name = "tendering"
    ; description = "This is just a test for the tendering concept"
    }
  = service
