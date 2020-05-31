open Sexplib.Conv

type ptype =
  | String
  | Int
  | Float
  | List   of ptype
[@@deriving sexp, show]

type property = string * ptype [@@deriving sexp, show]

type t = { properties : property list } [@@deriving show]

let t_of_sexp sexp =
  let open Sexplib.Sexp in
  let get_properties = function
    | [ Atom "properties"; List props ] | Atom "properties" :: props ->
        { properties = props |> List.map property_of_sexp }
    | _ ->
        failwith "shit"
  in
  match sexp with List lst -> get_properties lst | _ -> failwith "shit"


let%expect_test _ =
  let src = {|(properties (id string) (key string))|} in
  src
  |> Parsexp.Single.parse_string_exn
  |> t_of_sexp
  |> show
  |> print_endline ;
  [%expect
    {|
    { Structures.properties =
      [("id", Structures.String); ("key", Structures.String)] } |}]

let%expect_test _ =
  let src = {|(properties ((id int) (key string)))|} in
  src
  |> Parsexp.Single.parse_string_exn
  |> t_of_sexp
  |> show
  |> print_endline ;
  [%expect
    {|
    { Structures.properties =
      [("id", Structures.Int); ("key", Structures.String)] } |}]
