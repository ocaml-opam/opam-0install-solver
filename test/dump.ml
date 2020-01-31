(* For every package name in the opam repository, solve for that package and
   collect the results in a CSV file. *)

module Name = OpamPackage.Name

let results_file = "dump.csv"

let pp_pkg = Fmt.of_to_string OpamPackage.to_string

let pp_result f = function
  | Ok sels -> Fmt.pf f "%a" Fmt.(list ~sep:(unit " ") pp_pkg) (Opam_zi.packages_of_result sels)
  | Error _ -> Fmt.pf f "NO-SOLUTION"

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let path proc = Printf.sprintf "dump.proc-%d.csv" proc

let dump_slice ~available ~st proc start finish =
  match Unix.fork () with
  | 0 -> (* We are the child *)
    begin try
        let total = finish - start in
        let ch = open_out (path proc) in
        let f = Format.formatter_of_out_channel ch in
        let start_time = Unix.gettimeofday () in
        for i = start to finish - 1 do
          let off = i - start in
          if off mod 10 = 0 then Fmt.pr "Process %d completed %d/%d packages@." proc off total;
          let name = available.(i) in
          let constraints = OpamPackage.Name.Map.empty in
          let context = Opam_zi.create ~constraints st in
          let t0 = Unix.gettimeofday () in
          let r = Opam_zi.solve context [name] in
          let t1 = Unix.gettimeofday () in
          Fmt.pf f "%s, %.4f, %a@." (OpamPackage.Name.to_string name) (t1 -. t0) pp_result r
        done;
        let end_time = Unix.gettimeofday () in
        close_out ch;
        Fmt.pr "Process %d finished (%.2f packages / second)@." proc (float_of_int total /. (end_time -. start_time));
        exit 0
      with ex ->
        print_endline (Printexc.to_string ex);
        exit 1
    end
  | child -> child

let run n_cores =
  let t0 = Unix.gettimeofday () in
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  ignore (OpamStateConfig.load_defaults root);
  OpamStd.Config.init ();
  OpamStateConfig.init ();
  OpamClientConfig.opam_init ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  let rt = OpamRepositoryState.load `Lock_none gt in
  let st = OpamSwitchState.load_virtual gt rt in
  let t1 = Unix.gettimeofday () in
  Fmt.pr "Opam library initialised in %.2f s@." (t1 -. t0);
  let available = Lazy.force st.OpamStateTypes.available_packages
                  |> OpamPackage.Set.to_seq
                  |> Seq.map OpamPackage.name
                  |> Name.Set.of_seq |> Name.Set.to_seq
                  |> Array.of_seq in
  let pkgs_per_core = float_of_int (Array.length available) /. float_of_int n_cores |> Float.ceil |> int_of_float in
  let rec aux acc i =
    if i = n_cores then List.rev acc
    else (
      let finish = if i = n_cores - 1 then Array.length available else (i + 1) * pkgs_per_core in
      let child = dump_slice ~available ~st i (i * pkgs_per_core) finish in
      aux (child :: acc) (i + 1)
    )
  in
  let t0 = Unix.gettimeofday () in
  let children = aux [] 0 in
  let results = List.map waitpid_non_intr children in
  let t1 = Unix.gettimeofday () in
  results |> List.iteri (fun i (_pid, r) ->
      if r = Unix.WEXITED 0 then Fmt.pr "%d: OK@." i
      else Fmt.pr "%d: failed@." i
    );
  let time = t1 -. t0 in
  Fmt.pr "Finished in %.1f s (%.2f packages / second)@." time (float_of_int (Array.length available) /. time);
  let ch = open_out results_file in
  for i = 0 to n_cores - 1 do
    let part = open_in (path i) in
    let len = in_channel_length part in
    let data = really_input_string part len in
    close_in part;
    output_string ch data;
  done;
  close_out ch;
  Fmt.pr "Wrote %S@." results_file

let () =
  match Sys.argv with
  | [| _; n_cores |] -> run (int_of_string n_cores)
  | _ -> Fmt.epr "usage: dump n_cores@."; exit 1
