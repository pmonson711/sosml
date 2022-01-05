type map_name = string

type t = Undefined of map_name * Sort.sort list

let section_name = "map"

let mcrl2_of_t = function
  | Undefined (n, lst) ->
      Printf.sprintf "%s : %s ;" n
        (List.map Sort.mcrl2_of_sort' lst |> String.concat " -> ")

let section_of_t_list lst =
  let body =
    List.fold_left
      (fun acc x -> acc ^ Printf.sprintf "  %s\n" @@ mcrl2_of_t x)
      "" lst
  in
  Printf.sprintf "%s\n%s" section_name body

let write_out ?(printer = Printf.fprintf stdout) (lst : t list) =
  printer "%s" (section_of_t_list lst)

let%expect_test _ = write_out [] ; [%expect {| map |}]

let%expect_test _ =
  write_out [Undefined ("a_to_b", Sort.[Ref "abc"; Ref "dfe"])] ;
  [%expect {|
    map
      a_to_b : abc -> dfe ; |}]
