type rel = IsA of string [@@deriving show]

type binding =
  | Name of string * string
  | SexpBinding of string * Sexplib.Sexp.t
[@@deriving show]

type t =
  { name: string
  ; is_a: rel
  ; bindings: binding list
  ; protocol: Protocol.t list }
[@@deriving show]

let ( % ) f g x = g (f x)

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  let open Sexplib.Sexp in
  let find_name = function List (Atom n :: tl) -> Some (n, tl) | _ -> None in
  let find_is_a =
    List.find_map (function
      | List [Atom "is_a"; Atom tl] ->
          Some (IsA tl)
      | _ ->
          None)
  in
  let find_bindings =
    let is_binding str =
      try Scanf.sscanf str "%s@_is" Option.some with
      | End_of_file | Scanf.Scan_failure _ ->
          None
    in
    List.filter_map (function
      | List (Atom n :: tl) -> (
        match is_binding n with Some x -> Some (x, List tl) | None -> None )
      | _ ->
          None)
    % List.map (fun (x, tl) -> SexpBinding (x, tl))
  in
  let find_protocols lst =
    lst
    |> List.find_map (function
         | List (Atom "protocol" :: tl) ->
             Some tl
         | _ ->
             None)
    |> Option.value ~default:[]
    |> List.map Protocol.t_of_sexp
  in
  let name, rest = find_name sexp |> Option.get in
  let is_a = find_is_a rest |> Option.get in
  let bindings = find_bindings rest in
  let protocol = find_protocols rest in
  {name; is_a; bindings; protocol}

let get_key sexp =
  let open Sexplib.Sexp in
  let find lst =
    List.find_map
      (function List (Atom "stores" :: tl) -> Some tl | _ -> None)
      lst
  in
  match sexp with List lst -> find lst | _ -> None

let get_key_exn sexp = get_key sexp |> Option.get

let%expect_test _ =
  let wrap = Printf.sprintf "(%s)" in
  let src =
    {|
(name abc)
(stores
  (tender_aggregate_stream
    (is_a event_store)
    (key_is tender_id) ; allows the event store to accept anything that is a struct with a tender_id property
    (protocol (http 2)))
  (tender_aggregate_snapshot
    (is_a key_value)
    (key_is relay_reference_number) )
)

(abstraction
  (event_store (is_a 'key -> 'a list) ; assume a list per 'key
               (operation write (state 'key snoc)) ; implies state <| 'a -- but polymorphics can't be well supported so this implies the events are structs also, PM
               (operation read  (state 'key identity)) ) ; assumes we just return the entire list
  (key_value (is_a 'key -> 'value)
  	     (operation get (state 'key access))
	     (operation put (state 'key set))))
    |}
  in
  src |> wrap |> Parsexp.Single.parse_string_exn |> get_key_exn
  |> List.map (t_of_sexp % show)
  |> List.iter print_endline ;
  [%expect
    {|
    { Stores.name = "tender_aggregate_stream"; is_a = (Stores.IsA "event_store");
      bindings = [(Stores.SexpBinding ("key", (tender_id)))];
      protocol = [{ Protocol.name = "http"; version = "2" }] }
    { Stores.name = "tender_aggregate_snapshot"; is_a = (Stores.IsA "key_value");
      bindings = [(Stores.SexpBinding ("key", (relay_reference_number)))];
      protocol = [] } |}]
