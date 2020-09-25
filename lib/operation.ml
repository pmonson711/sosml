type input =
  | Noinput
  | Reference of string
[@@deriving show, eq]

type output =
  | NoOutput
  | Reference of string
[@@deriving show, eq]

type bind = string [@@deriving show, eq]

type guard = string list [@@deriving show, eq]

type rexp =
  | String  of string
  | MapTo   of string
  | FunCall of string
  | OpCall  of string list
[@@deriving show, eq]

type lexp =
  | Let    of bind * rexp
  | When   of guard list
  | OpCall of string list
[@@deriving show, eq]

type t =
  { name : string
  ; input : input
  ; output : output
  }
[@@deriving show, eq]

let get_key sexp =
  let open Sexplib.Sexp in
  let find lst =
    List.find_map
      (function List (Atom "operations" :: tl) -> Some tl | _ -> None)
      lst
  in
  match sexp with List lst -> find lst | _ -> None

let get_key_exn sexp = get_key sexp |> Option.get

let ( % ) f g x = g (f x)

