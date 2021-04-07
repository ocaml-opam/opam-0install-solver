


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
    v "4.13.0", Ok (OpamFile.OPAM.empty);
    v "4.12.0", Ok (OpamFile.OPAM.empty);
    v "4.11.0", Ok (OpamFile.OPAM.empty);
  ];
  n "ocaml-base-compiler", [
    v "4.13.0", Ok (o {|"ocaml" { = "4.13.0" } |});
    v "4.12.0", Ok (o {|"ocaml" { = "4.12.0" } |});
    v "4.11.0", Ok (o {|"ocaml" { = "4.11.0" } |});
    v "4.10.0", Error ();
  ];
  n "irmin-watcher", [
    v "0.1.2", Error ();
    v "0.3.0", Error ();
  ];
  n "ppxlib", [
    v "0.1.0", Ok (o {|"ocaml" { = "4.11.0" } |});
    v "0.3.1", Error ();
    v "0.22.0", Ok (OpamFile.OPAM.read_from_string {|
    opam-version: "2.0"
    depends: [
      "ppxlib"
      "irmin-watcher"
      "ocaml" { = "4.12.0"}
    ]
    |});
  ];
  n "irmin", [
    v "1.0.0", Ok (OpamFile.OPAM.read_from_string {|
opam-version: "2.0"
depends: [
  "ppxlib"
  "irmin-watcher"
  "ocaml-base-compiler" { >= "4.12.0"}
]
|})
  ]
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
  let failure = 
    Solver.solve () [OpamPackage.Name.of_string "irmin"]
    |> Result.map_error Solver.diagnostics 
    |> Result.map (fun s ->s |> Solver.packages_of_result |> List.map OpamPackage.to_string |> String.concat "\n")
  in
  Alcotest.(check (result string string)) "failure solution is minimal" (Error {|Can't find all required versions.
Selected: irmin.1.0.0 ppxlib.0.1.0
- irmin-watcher -> (problem)
    No usable implementations:
      irmin-watcher.0.3.0: unavailable
      irmin-watcher.0.1.2: unavailable|}) failure

let solver_suite = [
  ("find_minimal_solution", `Quick, test_find_minimal_solution)
]

let () = 
Alcotest.run "opam-0install" [
  ("solver", solver_suite)
]