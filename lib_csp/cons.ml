type cons_name = string

type t = Typed of cons_name * Sort.sort list

let section_name = "cons"

let mcrl2_of_t = function
  | Typed (str, srt) ->
      Printf.sprintf "%s : %s ;" str
      @@ String.concat " -> "
      @@ List.map Sort.mcrl2_of_sort' srt

let section_of_t_list lst =
  let body =
    List.fold_left
      (fun acc x -> acc ^ Printf.sprintf "  %s\n" (mcrl2_of_t x))
      "" lst
  in
  Printf.sprintf "%s\n%s" section_name body

let write_out ?(printer = Printf.fprintf stdout) (lst : t list) =
  printer "%s" (section_of_t_list lst)

let%expect_test _ = write_out [] ; [%expect "cons"]

let%expect_test _ =
  write_out [Typed ("next", Sort.[Ref "Id"; Ref "Id"])] ;
  [%expect {|
    cons
      next : Id -> Id ;|}]
