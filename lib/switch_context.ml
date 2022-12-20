type rejection = UserConstraint of OpamFormula.atom

type t = {
  st : OpamStateTypes.unlocked OpamStateTypes.switch_state;           (* To load the opam files *)
  pkgs : OpamTypes.version_set OpamTypes.name_map;                    (* All available versions *)
  constraints : OpamFormula.version_constraint OpamTypes.name_map;    (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
  prefer_oldest : bool;
}

let load t pkg =
  try OpamSwitchState.opam t.st pkg
  with Not_found ->
    failwith (Format.asprintf "Package %S not found!" (OpamPackage.to_string pkg))

let user_restrictions t name =
  OpamPackage.Name.Map.find_opt name t.constraints

let env t pkg v =
  match List.mem v OpamPackageVar.predefined_depends_variables with
  | true -> None
  | false -> (
    match OpamPackageVar.resolve_switch ~package:pkg t.st v with
    | Some _ as resolved -> resolved
    | None -> (
      match OpamVariable.Full.package v with
      | None -> (
        (* a local variable that is unknown *)
        OpamConsole.warning "Unknown variable %S" (OpamVariable.Full.to_string v);
        None)
      | Some pkg_name ->
        let installed_package = OpamPackage.Set.find_opt (fun package ->
            OpamPackage.Name.equal (OpamPackage.name package) pkg_name)
          t.st.installed
        in
        match installed_package with
        | None ->
          (* [v] refers to a package that is not installed in the switch *)
          None
        | Some package -> (
          let unscoped = OpamVariable.Full.(self (variable v)) in
          match OpamPackageVar.resolve_switch ~package t.st unscoped with
          | None ->
            OpamConsole.warning "Unknown variable %S" (OpamVariable.Full.to_string v);
            None
          | Some _ as resolved -> resolved
        )
      )
  )

let filter_deps t pkg f =
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false ~default:false

let sort_versions t versions =
  if t.prefer_oldest then
    versions
  else
    List.rev versions

let candidates t name =
  let user_constraints = user_restrictions t name in
  match OpamPackage.Name.Map.find_opt name t.pkgs with
  | Some versions ->
    OpamPackage.Version.Set.elements versions
    |> sort_versions t       (* Higher versions are preferred. *)
    |> List.map (fun v ->
        match user_constraints with
        | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
          v, Error (UserConstraint (name, Some test))
        | _ ->
          let opam = load t (OpamPackage.create name v) in
          (* Note: [OpamStateTypes.available_packages] filters out unavailable packages for us. *)
          v, Ok opam
      )
  | None ->
    OpamConsole.log "opam-0install" "Package %S not found!" (OpamPackage.Name.to_string name);
    []

let pp_rejection f = function
  | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)

let create ?(prefer_oldest=false) ?(test=OpamPackage.Name.Set.empty) ~constraints st =
  let pkgs = Lazy.force st.OpamStateTypes.available_packages |> OpamPackage.to_map in
  { st; pkgs; constraints; test; prefer_oldest }
