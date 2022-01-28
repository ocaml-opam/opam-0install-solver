let universe =
  Cudf.load_universe [
    {Cudf.default_package with package = "a"; version = 1};
    {Cudf.default_package with package = "a"; version = 2};
    {Cudf.default_package with package = "a"; version = 3};
    {Cudf.default_package with package = "a"; version = 4};

    {Cudf.default_package with package = "b"; version = 1};
    {Cudf.default_package with package = "b"; version = 2; pkg_extra = [("avoid-version", `Int 1)]};
    {Cudf.default_package with package = "b"; version = 3; pkg_extra = [("avoid-version", `Int 0)]};
    {Cudf.default_package with package = "b"; version = 4};

    {Cudf.default_package with package = "c"; version = 1; pkg_extra = [("avoid-version", `Int 1)]};
    {Cudf.default_package with package = "c"; version = 2};
    {Cudf.default_package with package = "c"; version = 3};
    {Cudf.default_package with package = "c"; version = 4; pkg_extra = [("avoid-version", `Int 0)]};

    {Cudf.default_package with package = "d"; version = 1; pkg_extra = [("avoid-version", `Int 0)]};
    {Cudf.default_package with package = "d"; version = 2};
    {Cudf.default_package with package = "d"; version = 3};
    {Cudf.default_package with package = "d"; version = 4; pkg_extra = [("avoid-version", `Int 1)]};

    {Cudf.default_package with package = "e"; version = 1};
    {Cudf.default_package with package = "e"; version = 2; installed = true};
    {Cudf.default_package with package = "e"; version = 3};
    {Cudf.default_package with package = "e"; version = 4};
  ]

let solve ?prefer_oldest ?handle_avoid_version ?keep_installed req =
  let x = Opam_0install_cudf.create ?prefer_oldest ?handle_avoid_version ?keep_installed ~constraints:[] universe in
  match Opam_0install_cudf.solve x req with
  | Ok sel -> Ok (Opam_0install_cudf.packages_of_result sel)
  | Error diag -> Error (Opam_0install_cudf.diagnostics ~verbose:true diag)

let simple_solve () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("a", 4)])
    (solve [("a", `Essential)])

let simple_oldest () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("a", 1)])
    (solve ~prefer_oldest:true [("a", `Essential)])

let simple_avoid_1 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("b", 4)])
    (solve ~handle_avoid_version:true [("b", `Essential)])

let oldest_avoid_1 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("b", 1)])
    (solve ~prefer_oldest:true ~handle_avoid_version:true [("b", `Essential)])

let simple_avoid_2 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("c", 4)])
    (solve ~handle_avoid_version:true [("c", `Essential)])

let oldest_avoid_2 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("c", 2)])
    (solve ~prefer_oldest:true ~handle_avoid_version:true [("c", `Essential)])

let simple_avoid_3 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("d", 3)])
    (solve ~handle_avoid_version:true [("d", `Essential)])

let oldest_avoid_3 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("d", 1)])
    (solve ~handle_avoid_version:true ~prefer_oldest:true [("d", `Essential)])

let keep_installed_1 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("e", 2)])
    (solve ~keep_installed:true [("e", `Essential)])

let keep_installed_2 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("e", 2)])
    (solve ~keep_installed:true [("e", `Recommended)])

let keep_installed_3 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("e", 4)])
    (solve [("e", `Essential)])

let keep_installed_4 () =
  Alcotest.(check (result (list (pair string int)) string))
    "equal" (Ok [("e", 4)])
    (solve [("e", `Recommended)])

let () =
  Alcotest.run "cudf"
    [
      ( "simple solve",
        [
          Alcotest.test_case "normal" `Quick simple_solve;
          Alcotest.test_case "oldest" `Quick simple_oldest;
        ] );
      ( "avoid-version",
        [
          Alcotest.test_case "normal 1" `Quick simple_avoid_1;
          Alcotest.test_case "oldest 1" `Quick oldest_avoid_2;
          Alcotest.test_case "normal 2" `Quick simple_avoid_2;
          Alcotest.test_case "oldest 2" `Quick oldest_avoid_2;
          Alcotest.test_case "normal 3" `Quick simple_avoid_3;
          Alcotest.test_case "oldest 3" `Quick oldest_avoid_3;
        ] );
      ( "keep-installed",
        [
          Alcotest.test_case "normal 1" `Quick keep_installed_1;
          Alcotest.test_case "normal 2" `Quick keep_installed_2;
          Alcotest.test_case "normal 3" `Quick keep_installed_3;
          Alcotest.test_case "normal 4" `Quick keep_installed_4;
        ] );
    ]
