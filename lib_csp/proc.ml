type proc_name = string

type t =
  | Untyped of string
  | Typed of string * Sort.sort list
