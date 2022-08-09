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

let solve context pkgs =
  let req = requirements ~context pkgs in
  match Solver.do_solve ~closest_match:false req with
  | Some sels -> Ok sels
  | None -> Error req

let packages_of_result sels =
  sels
  |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
  |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))

module Raw_diagnostics = struct
  type restriction = Input.restriction = {
    kind : [`Ensure | `Prevent];
    expr : (Cudf_types.relop * Cudf_types.version) list;
  }

  type role =
    | Real of Cudf_types.pkgname
    | Virtual of impl list
  and real_impl = {
    pkg : Cudf.package;
    requires : dependency list;
  }
  and dependency = {
    drole : role;
    importance : [`Essential | `Recommended | `Restricts];
    restrictions : restriction list;
  }
  and impl =
    | RealImpl of real_impl
    | VirtualImpl of dependency list
    | Reject of (Cudf_types.pkgname * Cudf_types.version)
    | Dummy

  type rejection_reason =
    | ModelRejection of Cudf_types.vpkg
    | FailsRestriction of restriction
    | DepFailsRestriction of dependency * restriction
    | ConflictsRole of role
    | DiagnosticsFailure of string

  type reject = impl * rejection_reason

  type note =
    | UserRequested of restriction
    | ReplacesConflict of role
    | ReplacedByConflict of role
    | Restricts of role * impl * restriction list
    | Feed_problem of string

  type t = {
    role : role;
    selected_impl : impl option;
    notes : note list;
    candidates : (reject list * [`All_unusable | `No_candidates | `Conflicts]) option;
  }

  let rec map_role = function
    | Input.Real {context = _; name} -> Real name
    | Input.Virtual (_, impls) -> Virtual (List.map map_impl impls)
  and map_impl = function
    | Input.RealImpl {pkg; requires} -> RealImpl {pkg; requires = List.map map_dependency requires}
    | Input.VirtualImpl (_, dependencies) -> VirtualImpl (List.map map_dependency dependencies)
    | Input.Reject pkg -> Reject pkg
    | Input.Dummy -> Dummy
  and map_dependency {drole; importance; restrictions} =
    { drole = map_role drole; importance; restrictions}

  let map_note = function
    | Diagnostics.Note.UserRequested restriction -> UserRequested restriction
    | Diagnostics.Note.ReplacesConflict role -> ReplacesConflict (map_role role)
    | Diagnostics.Note.ReplacedByConflict role -> ReplacedByConflict (map_role role)
    | Diagnostics.Note.Restricts (role, impl, restrictions) -> Restricts (map_role role, map_impl impl, restrictions)
    | Diagnostics.Note.RequiresCommand _ -> assert false (* NOTE: the current implementation does not have any commands *)
    | Diagnostics.Note.Feed_problem msg -> Feed_problem msg

  let map_reason = function
    | `Model_rejection (Context.UserConstraint rejection) -> ModelRejection rejection
    | `FailsRestriction restriction -> FailsRestriction restriction
    | `DepFailsRestriction (dependency, restriction) -> DepFailsRestriction (map_dependency dependency, restriction)
    | `MachineGroupConflict _ -> assert false (* NOTE: the current implementation does not have any machine groups *)
    | `ClassConflict _ -> assert false (* NOTE: the current implementation does not have any class-conflicts *)
    | `ConflictsRole role -> ConflictsRole (map_role role)
    | `MissingCommand _ -> assert false (* NOTE: the current implementation does not have any commands *)
    | `DiagnosticsFailure msg -> DiagnosticsFailure msg

  let map_reject (impl, reason) =
    (map_impl impl, map_reason reason)

  let map_candidates (rejects, kind) =
    (List.map map_reject rejects, kind)

  let get_aux req =
    Solver.do_solve req ~closest_match:true
    |> Option.get

  let get req =
    get_aux req |>
    Diagnostics.of_result |>
    Solver.Output.RoleMap.bindings |>
    List.map (fun (_role, component) ->
      let selected_impl = Option.map map_impl (Diagnostics.Component.selected_impl component) in
      {
        role = map_role (Diagnostics.Component.role component);
        selected_impl;
        notes = List.map map_note (Diagnostics.Component.notes component);
        candidates = begin match selected_impl with
          | None -> Some (map_candidates (Diagnostics.Component.rejects component))
          | Some _ -> None
        end;
      }
    )
end

let diagnostics ?verbose req =
  Raw_diagnostics.get_aux req |> Diagnostics.get_failure_reason ?verbose
