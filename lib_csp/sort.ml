type sort_name = string [@@deriving show]

type struct_name = string [@@deriving show]

type accessor_name = string [@@deriving show]

type sort =
  | Bool
  | Int
  | Pos
  | Nat
  | Real
  | List of sort
  | Set of sort
  | FSet of sort
  | Bag of sort
  | FBag of sort
  | Id of string
  | Ref of string
  | Struct of struct_name * property list
  | Variants of sort list

and property = accessor_name * sort [@@deriving show]

type t =
  | Untyped of sort_name
  | Typed of sort_name * sort

module T : sig
  val bool : string -> t

  val int : string -> t

  val discreet : string -> string list -> t

  val struct' : string -> property list -> t

  val variants : string -> sort list -> t
end = struct
  let bool n = Typed (n, Bool)

  let int n = Typed (n, Int)

  let discreet name values =
    Typed (name, Variants (List.map (fun x -> Id x) values))

  let struct' name props = Typed (name, Struct (name, props))

  let variants name sorts = Typed (name, Variants sorts)
end

let section_name = "sort"

let variant_seperator = " | "

let property_seperator = ", "

let rec mcrl2_of_sort = function
  | Variants lst ->
      let s' = mcrl2_of_sort' (Variants lst) in
      Printf.sprintf "struct %s" s'
  | Struct (s, l) ->
      let s' = mcrl2_of_sort' (Struct (s, l)) in
      Printf.sprintf "struct %s" s'
  | s ->
      mcrl2_of_sort' s

and mcrl2_of_sort' = function
  | Bool ->
      "Bool"
  | Int ->
      "Int"
  | Pos ->
      "Pos"
  | Nat ->
      "Nat"
  | Real ->
      "Real"
  | List s ->
      Printf.sprintf "List(%s)" (mcrl2_of_sort' s)
  | Set s ->
      Printf.sprintf "Set(%s)" (mcrl2_of_sort' s)
  | FSet s ->
      Printf.sprintf "FSet(%s)" (mcrl2_of_sort' s)
  | Bag s ->
      Printf.sprintf "Bag(%s)" (mcrl2_of_sort' s)
  | FBag s ->
      Printf.sprintf "FBag(%s)" (mcrl2_of_sort' s)
  | Id s ->
      s
  | Ref s ->
      s
  | Variants lst ->
      Printf.sprintf "%s"
        (List.map mcrl2_of_sort' lst |> String.concat variant_seperator)
  | Struct (s, l) ->
      Printf.sprintf "%s(%s) ?is_%s" s
        (List.map mcrl2_of_property l |> String.concat property_seperator)
        (String.lowercase_ascii s)

and mcrl2_of_property (v : property) =
  match v with n, s -> Printf.sprintf "%s: %s" n (mcrl2_of_sort' s)

let mcrl2_of_t = function
  | Untyped s ->
      Printf.sprintf "%s ;" s
  | Typed (s, p) ->
      Printf.sprintf "%s= %s ;" s (mcrl2_of_sort p)

let section_of_t_list lst =
  let body =
    List.fold_left
      (fun acc x -> acc ^ Printf.sprintf "  %s\n" (mcrl2_of_t x))
      "" lst
  in
  Printf.sprintf "%s\n%s" section_name body

let write_out ?(printer = Printf.fprintf stdout) (lst : t list) =
  printer "%s" (section_of_t_list lst)

let%expect_test "untyped" =
  write_out [Untyped "thing"] ;
  [%expect {|
      sort
        thing ; |}]

let%expect_test "bool" =
  write_out [T.(bool "thing")] ;
  [%expect {|
      sort
        thing= Bool ; |}]

let%expect_test "bool" =
  write_out [T.(int "thing")] ;
  [%expect {|
      sort
        thing= Int ; |}]

let%expect_test "discreet" =
  write_out [T.(discreet "Time" ["time_1"; "time_2"])] ;
  [%expect {|
      sort
        Time= struct time_1 | time_2 ; |}]

let%expect_test "props" =
  write_out
    [ T.(struct' "Shipper" [("ready_date", Ref "ReadyDate")])
    ; T.(
        struct' "Receiver"
          [ ("delivery_by_date", Ref "DeliverByDate")
          ; ("delivery_by_time", Ref "DeleveryByTime") ]) ] ;
  [%expect
    {|
      sort
        Shipper= struct Shipper(ready_date: ReadyDate) ?is_shipper ;
        Receiver= struct Receiver(delivery_by_date: DeliverByDate, delivery_by_time: DeleveryByTime) ?is_receiver ; |}]

let%expect_test "props" =
  write_out
    [ T.(
        variants "PlanState"
          [ Id "no_plan"
          ; Struct ("a", [("delivery_by_time", Ref "DeleveryByTime")]) ]) ] ;
  [%expect
    {|
    sort
      PlanState= struct no_plan | a(delivery_by_time: DeleveryByTime) ?is_a ; |}]
