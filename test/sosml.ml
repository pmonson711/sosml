let truth () = Alcotest.(check bool) "is true" true true

let () =
  let open Alcotest in
  run "Utils" [ ("truth case", [ test_case "Is expected" `Quick truth ]) ]
