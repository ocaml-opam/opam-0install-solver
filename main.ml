let pp_pkg = Fmt.of_to_string OpamPackage.to_string

let select = function
  | [] -> OpamConsole.error "No packages requested!"; `Bad_arguments
  | spec ->
    let t0 = Unix.gettimeofday () in
    let root = OpamStateConfig.opamroot () in
    OpamFormatConfig.init ();
    ignore (OpamStateConfig.load_defaults root);
    OpamStd.Config.init ();
    OpamStateConfig.init ();
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_none gt @@ fun st ->
    let t1 = Unix.gettimeofday () in
    OpamConsole.note "Opam library initialised in %.2f s" (t1 -. t0);
    (* Collect any user-provided constraints from the command-line arguments: *)
    let constraints =
      spec
      |> List.filter_map (function
          | _, None -> None
          | x, Some y -> Some (x, y)
        )
      |> OpamPackage.Name.Map.of_list in
    let context = Opam_zi.create ~constraints st in
    let pkgs = List.map fst spec in
    (* Try to find a solution: *)
    let t0 = Unix.gettimeofday () in
    let r = Opam_zi.solve context pkgs in
    let t1 = Unix.gettimeofday () in
    match r with
    | Ok sels ->
      Fmt.pr "%a@." Fmt.(list ~sep:(unit " ") pp_pkg) (Opam_zi.packages_of_result sels);
      OpamConsole.note "Solve took %.2f s" (t1 -. t0);
      `Success
    | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Opam_zi.diagnostics problem);
      OpamConsole.note "Eliminated all possibilities in %.2f s" (t1 -. t0);
      `No_solution

open Cmdliner

(* name * version constraint
   Based on version in opam-client, which doesn't seem to be exposed. *)
let atom =
  let parse str =
    let re = Re.(compile @@ seq [
        bos;
        group @@ rep1 @@ diff any (set ">=<.!");
        group @@ alt [ seq [ set "<>"; opt @@ char '=' ];
                       set "=."; str "!="; ];
        group @@ rep1 any;
        eos;
      ]) in
    try
      let sub = Re.exec re str in
      let sname = Re.Group.get sub 1 in
      let sop = Re.Group.get sub 2 in
      let sversion = Re.Group.get sub 3 in
      let name = OpamPackage.Name.of_string sname in
      let sop = if sop = "." then "=" else sop in
      let op = OpamLexer.relop sop in
      let version = OpamPackage.Version.of_string sversion in
      `Ok (name, Some (op, version))
    with Not_found | Failure _ | OpamLexer.Error _ ->
    try `Ok (OpamPackage.Name.of_string str, None)
    with Failure msg -> `Error msg
  in
  let print ppf atom =
    Fmt.string ppf (OpamFormula.short_string_of_atom atom) in
  parse, print

let spec =
  Arg.pos_all atom [] @@ Arg.info []

let cmd =
  let doc = "Select opam packages using 0install backend" in
  Term.(const select $ Arg.value spec),
  Term.info "opam-zi" ~doc

let () =
  match Term.eval cmd with
  | `Ok reason -> exit (OpamStd.Sys.get_exit_code reason)
  | x -> Term.exit x
