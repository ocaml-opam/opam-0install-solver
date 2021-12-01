module Context = struct
  type rejection = UserConstraint of Cudf_types.vpkg

  type t = {
    universe : Cudf.universe;
    constraints : (Cudf_types.pkgname * (Cudf_types.relop * Cudf_types.version)) list;
    prefer_oldest : bool;
  }

  let user_restrictions t name =
    List.fold_left (fun acc (name', c) ->
      if String.equal name name' then
        c :: acc
      else
        acc
    ) [] t.constraints

  let version_compare t pkg1 pkg2 =
    if t.prefer_oldest then
      compare (pkg1.Cudf.version : int) pkg2.Cudf.version
    else
      compare (pkg2.Cudf.version : int) pkg1.Cudf.version

  let candidates t name =
    let user_constraints = user_restrictions t name in
    match Cudf.lookup_packages t.universe name with
    | [] ->
        [] (* Package not found *)
    | versions ->
        List.fast_sort (version_compare t) versions (* Higher versions are preferred. *)
        |> List.map (fun pkg ->
          let rec check_constr = function
            | [] -> (pkg.Cudf.version, Ok pkg)
            | ((op, v)::c) ->
                if Model.fop op pkg.Cudf.version v then
                  check_constr c
                else
                  (pkg.Cudf.version, Error (UserConstraint (name, Some (op, v))))  (* Reject *)
          in
          check_constr user_constraints
        )

  let print_constr = function
    | None -> ""
    | Some (`Eq, v) -> "="^string_of_int v
    | Some (`Neq, v) -> "!="^string_of_int v
    | Some (`Geq, v) -> ">="^string_of_int v
    | Some (`Gt, v) -> ">"^string_of_int v
    | Some (`Leq, v) -> "<="^string_of_int v
    | Some (`Lt, v) -> "<"^string_of_int v

  let pp_rejection f = function
    | UserConstraint (name, c) -> Format.fprintf f "Rejected by user-specified constraint %s%s" name (print_constr c)
end

module Input = Model.Make(Context)

let requirements ~context pkgs =
  let role =
    let impl = Input.virtual_impl ~context ~depends:pkgs () in
    Input.virtual_role [impl]
  in
  { Input.role; command = None }

module Solver = Zeroinstall_solver.Make(Input)
module Diagnostics = Zeroinstall_solver.Diagnostics(Solver.Output)

type t = Context.t
type selections = Solver.Output.t
type diagnostics = Input.requirements   (* So we can run another solve *)

let create ?(prefer_oldest=false) ~constraints universe =
  { Context.universe; constraints; prefer_oldest }

let packages_of_result sels =
  sels
  |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
  |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))

let tagged_with_avoid_version pkg =
  List.exists (function
    | "avoid-version", `Bool b -> b
    | _ -> false
  ) pkg.Cudf.pkg_extra

let selection_contains_new_avoid_versions {Context.universe; _} selections =
  packages_of_result selections |>
  List.exists (fun (pkgname, v) ->
    let pkg = Cudf.lookup_package universe (pkgname, v) in
    let installed = Cudf.get_installed universe pkgname in
    tagged_with_avoid_version pkg &&
    not (List.exists tagged_with_avoid_version installed)
  )

let remove_new_avoid_versions {Context.universe; _} =
  let new_universe = Cudf.empty_universe ~size:(Cudf.universe_size universe) () in
  Cudf.iter_packages_by_name (fun pkg pkgs ->
    let installed = Cudf.get_installed universe pkg in
    let installed_with_avoid_version = List.exists tagged_with_avoid_version installed in
    if installed_with_avoid_version then
      List.iter (Cudf.add_package new_universe) pkgs
    else
      List.iter (fun pkg ->
        if not (tagged_with_avoid_version pkg) then
          Cudf.add_package new_universe pkg
      ) pkgs
  ) universe;
  new_universe

let solve context pkgs =
  let req = requirements ~context pkgs in
  match Solver.do_solve ~closest_match:false req with
  | Some sels ->
      if selection_contains_new_avoid_versions context sels then
        Ok sels
      else
        let universe_without_new_avoid_versions = remove_new_avoid_versions context in
        let context = { context with Context.universe = universe_without_new_avoid_versions } in
        let req = requirements ~context pkgs in
        begin match Solver.do_solve ~closest_match:false req with
        | Some sels -> Ok sels
        | None -> Ok sels
        end
  | None -> Error req

let diagnostics ?verbose req =
  Solver.do_solve req ~closest_match:true
  |> Option.get
  |> Diagnostics.get_failure_reason ?verbose
