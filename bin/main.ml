open Cmdliner

let info =
  let doc = "parsesr for sosml" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <pmonson711@gmail.com>" ]
  in
  Term.info
    "oevtstore"
    ~version:"%%%VERSION%%"
    ~doc
    ~exits:Term.default_exits
    ~man


let cmds = [ Read_print.cmd ]

type verb =
  | Normal
  | Quiet
  | Verbose

type copts =
  { debug : bool
  ; verb : verb
  }

let copts debug verb = { debug; verb }

let copts_t =
  let debug = Arg.(value & flag & info [ "debug" ]) in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = (Quiet, Arg.info [ "q"; "quiet" ] ~doc) in
    let doc = "Give verbose output." in
    let verbose = (Verbose, Arg.info [ "v"; "verbose" ] ~doc) in
    Arg.(last & vflag_all [ Normal ] [ quiet; verbose ])
  in
  Term.(const copts $ debug $ verb)


let default_cmd =
  let doc = "eventstore" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_common_options
    ; `P "These options are common to all commands"
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help for help on a single command"
    ; `Noblank
    ; `P "Use `$(mname) help patterns' for help on a pattern matching."
    ; `Noblank
    ; `P "Use `$(mname) help environment' for help on environment variables."
    ; `S Manpage.s_bugs
    ; `P "Check bug reports to <pmonson@echo.com>"
    ]
  in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t))
  , Term.info "main" ~version:"v0.1.0" ~doc ~sdocs ~exits ~man )


let () = Term.(exit @@ eval_choice default_cmd cmds)
