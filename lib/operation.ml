type input =
  | Noinput
  | Reference of string
[@@deriving show]

type output =
  | NoOutput
  | Reference of string
[@@deriving show]

type bind = string [@@deriving show]

type guard = string list [@@deriving show]

type rexp =
  | String  of string
  | MapTo   of string
  | FunCall of string
  | OpCall  of string list
[@@deriving show]

type lexp =
  | Let    of bind * rexp
  | When   of guard list
  | OpCall of string list
[@@deriving show]

type t =
  { name : string
  ; input : input
  ; output : output
  }
[@@deriving show]
