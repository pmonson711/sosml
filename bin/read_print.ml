let run =
  let example_dir = "examples" in
  let exp = Fpath.(v example_dir / "tender.sos") in
  match Bos.OS.File.read exp with
  | Ok sexp ->
      Sosml.Parser.(sexp |> parse_exn |> print_endline)
  | Error _ ->
      failwith "shit"

let cmd =
  let open Cmdliner in
  let doc = "Reads and prints example" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P doc] in
  (Term.(const run), Term.info "read_print" ~doc ~exits ~man)
