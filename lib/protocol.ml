type name = string [@@deriving show]

type version = string [@@deriving show]

type t =
  { name: name
  ; version: version }
[@@deriving show]

let t_of_sexp sexp =
  let open Sexplib.Sexp in
  match sexp with
  | List [Atom name; Atom version] ->
      {name; version}
  | _ ->
      failwith (Sexplib.Sexp.to_string_hum sexp)

let get_key sexp =
  let open Sexplib.Sexp in
  let find lst =
    List.find_map
      (function List (Atom "protocols" :: tl) -> Some tl | _ -> None)
      lst
  in
  match sexp with List lst -> find lst | _ -> None

let get_key_exn sexp = get_key sexp |> Option.get

let%expect_test _ =
  let wrap = Printf.sprintf "(%s)" in
  let src =
    {|
(name abc)
(protocols
  (http 1.1)
  (http 2)
  (amqp 1.9)
  (amqp 2)
)
    |}
  in
  src |> wrap |> Parsexp.Single.parse_string_exn |> get_key_exn
  |> List.map t_of_sexp |> List.map show |> List.iter print_endline ;
  [%expect
    {|
    { Protocol.name = "http"; version = "1.1" }
    { Protocol.name = "http"; version = "2" }
    { Protocol.name = "amqp"; version = "1.9" }
    { Protocol.name = "amqp"; version = "2" } |}]
