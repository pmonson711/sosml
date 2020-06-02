type t =
  [ `Parsed    of Sexplib.Sexp.t
  | `Rewritten of Sexplib.Sexp.t
  ]

val parse :
  string -> ([> `Parsed of Sexplib.Sexp.t ], Parsexp.Parse_error.t) result

val parse_exn : string -> [> `Parsed of Sexplib.Sexp.t ]

val print_endline : t -> unit

val get_version : [< `Parsed of Sexplib.Sexp.t ] -> Version.t

val get_service : [< `Rewritten of Sexplib.Sexp.t ] -> Service.t

val get_structures : t -> Structure.t list

val get_events : t -> Structure.t list
