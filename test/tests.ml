


let repository =
  let n = OpamPackage.Name.of_string in
  let v = OpamPackage.Version.of_string in
  let o deps = OpamFile.OPAM.read_from_string (Fmt.str {|
  opam-version: "2.0"
  depends: [
    %s
  ]
  |} deps)in
  OpamPackage.Name.Map.of_list [
  n "ocaml", [
    v "4.13.0", Error ();
    v "4.12.0", Ok (OpamFile.OPAM.empty);
  ];
  n "irmin-watcher", [
    v "0.3.0", Error ();
  ];
  n "ppxlib", [
    v "0.23.0", Ok (o {|"ocaml" { = "4.13.0"}|});
    v "0.22.0", Ok (o {|"ocaml" { = "4.12.0"}|});
  ];
]

module Test_context = struct

  type t = unit

  type rejection = unit

  let pp_rejection = Fmt.unit "unavailable"

  let candidates () name = OpamPackage.Name.Map.find_opt name repository |> Option.value ~default:[]

  let user_restrictions () _ = None

  let filter_deps () _ formula = OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false ~dev:false ~default:false formula

end

module Solver = Opam_0install.Solver.Make(Test_context)

let solve = Solver.solve ()

let test_find_minimal_solution () =
  let solve lst =
    lst
    |> List.map OpamPackage.Name.of_string
    |> Solver.solve ()
    |> Result.map_error Solver.diagnostics
    |> Result.map (fun s ->s |> Solver.packages_of_result |> List.map OpamPackage.to_string |> String.concat "\n")
  in

  Alcotest.(check (result string string)) "solving without the failing package works"
    (Ok "ocaml.4.12.0\nppxlib.0.22.0")
    (solve ["ppxlib"]);

  Alcotest.(check (result string string)) "failure solution is minimal"
    (Error {|Can't find all required versions.
Selected: ocaml.4.12.0 ppxlib.0.22.0
- irmin-watcher -> (problem)
    No usable implementations:
      irmin-watcher.0.3.0: unavailable|})
    (solve ["ppxlib"; "irmin-watcher"])

let solver_suite = [
  ("find_minimal_solution", `Quick, test_find_minimal_solution)
]

let () =
Alcotest.run "opam-0install" [
  ("solver", solver_suite)
]