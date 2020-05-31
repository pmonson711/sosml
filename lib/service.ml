open Sexplib.Conv

type t =
  { description : string
  ; name : string
  }
[@@sexp.allow_extra_fields] [@@deriving sexp]

let%expect_test _ =
  let description = "description" in
  let name = "name" in
  { description; name }
  |> sexp_of_t
  |> Sexplib.Sexp.to_string_mach
  |> print_endline ;
  [%expect {| ((description description)(name name)) |}]

let%expect_test "always prints name and description" =
  let description = "" in
  let name = "" in
  { description; name }
  |> sexp_of_t
  |> Sexplib.Sexp.to_string_mach
  |> print_endline ;
  [%expect {| ((description"")(name"")) |}]
