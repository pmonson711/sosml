open Sexplib.Conv

type t =
  { description : string
  ; name : string
  }
[@@sexp.allow_extra_fields] [@@deriving sexp, show, eq]
