open Sexplib.Conv

type ptype =
  | String
  | VString of string
  | Int
  | VInt    of int
  | Float
  | VFloat  of float
  | Option  of ptype
  | List    of ptype
  | Set     of ptype
[@@deriving sexp, show]

type property = string * ptype [@@deriving sexp, show]

type reference =
  { name : string
  ; reference_of : string
  }
[@@deriving show]

type alias =
  { name : string
  ; alias_of : ptype
  }
[@@deriving show]

type structure =
  { name : string
  ; properties : property list
  }
[@@deriving show]

type t =
  | Alias     of alias
  | Basic     of structure
  | Reference of reference
  | Variant   of string * structure
[@@deriving show]

let t_of_sexp sexp =
  let open Sexplib.Sexp in
  let get_properties name = function
    | [ Atom "properties"; List props ] ->
        Basic { name; properties = [ property_of_sexp (List props) ] }
    | Atom "properties" :: props ->
        Basic { name; properties = List.map property_of_sexp props }
    | [ Atom var; List (Atom "properties" :: props) ] ->
        Variant (var, { name; properties = List.map property_of_sexp props })
    | [ Atom "ref"; Atom reference ] ->
        Reference { name; reference_of = reference }
    | _ ->
        failwith (Sexplib.Sexp.to_string_hum sexp)
  in
  match sexp with
  | List [ Atom name; List lst ] ->
      [ get_properties name lst ]
  | List (Atom name :: lst) ->
      List.map
        (fun x ->
          match x with
          | List v     ->
              get_properties name v
          | Atom alias ->
              Alias { name; alias_of = ptype_of_sexp (Atom alias) })
        lst
  | _ ->
      failwith (Sexplib.Sexp.to_string_hum sexp)


let get_key name sexp =
  let open Sexplib.Sexp in
  let find lst =
    List.find_map
      (fun x ->
        match x with
        | List (Atom _name :: tl) when _name = name ->
            Some tl
        | _ ->
            None)
      lst
  in
  match sexp with List lst -> find lst | _ -> None


let get_key_exn name sexp = get_key name sexp |> Option.get

module Type : sig
  val string : ptype

  val static_string : string -> ptype

  val int : ptype

  val static_int : int -> ptype

  val float : ptype

  val static_float : float -> ptype

  val property : string -> ptype -> property

  val reference : string -> string -> t

  val alias : string -> ptype -> t

  val structure : string -> property list -> t

  val variant : string -> string -> property list -> t
end = struct
  let string = String

  let static_string string = VString string

  let int = Int

  let static_int int = VInt int

  let float = Float

  let static_float float = VFloat float

  let property name ptype = (name, ptype)

  let reference name reference_of = Reference { name; reference_of }

  let alias name alias_of = Alias { name; alias_of }

  let structure name properties = Basic { name; properties }

  let variant variant name properties =
    Variant (variant, { name; properties })
end

let%expect_test _ =
  let wrap = Printf.sprintf "(%s)" in
  let src =
    {|
(name abc)
(structures
  ( tender
      (properties (id string)) )
  ( tender1
      (properties (id string)(key (list string))) ) ;; TODO should accept (key string list)
  ( tender-variant
      (a (properties (id string)))
      (c (properties (id string)(key (list int))))
      (b (properties (id string) (key string))))
  ( tender-base string)
  ( tender-base-2 (ref tendering.tender) )
)
    |}
  in
  src
  |> wrap
  |> Parsexp.Single.parse_string_exn
  |> get_key_exn "structures"
  |> List.map t_of_sexp
  |> List.flatten
  |> List.iter (fun s -> s |> show |> print_endline) ;
  [%expect
    {|
    (Structure.Basic
       { Structure.name = "tender"; properties = [("id", Structure.String)] })
    (Structure.Basic
       { Structure.name = "tender1";
         properties =
         [("id", Structure.String); ("key", (Structure.List Structure.String))] })
    (Structure.Variant ("a",
       { Structure.name = "tender-variant";
         properties = [("id", Structure.String)] }
       ))
    (Structure.Variant ("c",
       { Structure.name = "tender-variant";
         properties =
         [("id", Structure.String); ("key", (Structure.List Structure.Int))] }
       ))
    (Structure.Variant ("b",
       { Structure.name = "tender-variant";
         properties = [("id", Structure.String); ("key", Structure.String)] }
       ))
    (Structure.Alias
       { Structure.name = "tender-base"; alias_of = Structure.String })
    (Structure.Reference
       { Structure.name = "tender-base-2"; reference_of = "tendering.tender" }) |}]
