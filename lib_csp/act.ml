type action_name = string

type t =
  | Untyped of action_name
  | Typed of action_name * Sort.sort
  | MultiTyped of action_name list * Sort.sort

let section_name = "act"

let mcrl2_of_t = function
  | Untyped str ->
      Printf.sprintf "%s ;" str
  | Typed (str, srt) ->
      Printf.sprintf "%s : %s ;" str (Sort.mcrl2_of_sort' srt)
  | MultiTyped (lst, srt) ->
      Printf.sprintf "%s : %s ;" (String.concat ", " lst)
        (Sort.mcrl2_of_sort' srt)

let section_of_t_list lst =
  let body =
    List.fold_left
      (fun acc x -> acc ^ Printf.sprintf "  %s\n" (mcrl2_of_t x))
      "" lst
  in
  Printf.sprintf "%s\n%s" section_name body

let write_out ?(printer = Printf.fprintf stdout) (lst : t list) =
  printer "%s" (section_of_t_list lst)

let%expect_test _ = write_out [] ; [%expect "act"]

let%expect_test _ =
  write_out [Untyped "empty"] ;
  [%expect {|
    act
      empty ; |}]

let%expect_test _ =
  write_out [Typed ("action", Sort.(List (Ref "abc")))] ;
  [%expect {|
    act
      action : List(abc) ; |}]

let%expect_test _ =
  write_out [MultiTyped (["action"; "action_b"], Sort.(List (Ref "abc")))] ;
  [%expect {|
    act
      action, action_b : List(abc) ; |}]
