let () =
  let open Alcotest in
  run "Utils" [Version_test.case; Operation_test.case; Service_test.case]
