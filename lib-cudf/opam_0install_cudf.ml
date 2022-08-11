type context = {
  universe : Cudf.universe;
  constraints : (Cudf_types.pkgname * (Cudf_types.relop * Cudf_types.version)) list;
  prefer_oldest : bool;
  handle_avoid_version : bool;
  fresh_id : int ref;
}

let tagged_with_avoid_version pkg =
  List.exists (function
    | "avoid-version", `Bool b -> b
    | _ -> false
  ) pkg.Cudf.pkg_extra

let version_rev_compare context pkg1 pkg2 =
  let rev_cmp () =
    if context.prefer_oldest then
      Int.compare pkg1.Cudf.version pkg2.Cudf.version
    else
      Int.compare pkg2.Cudf.version pkg1.Cudf.version
  in
  if context.handle_avoid_version then
    match tagged_with_avoid_version pkg1, tagged_with_avoid_version pkg2 with
    | true, true | false, false -> rev_cmp ()
    | true, false -> 1
    | false, true -> -1
  else
    rev_cmp ()

module Context = struct
  type rejection = UserConstraint of Cudf_types.vpkg

  type t = context

  let user_restrictions t name =
    List.fold_left (fun acc (name', c) ->
      if String.equal name name' then
        c :: acc
      else
        acc
    ) [] t.constraints

  let candidates t name =
    let user_constraints = user_restrictions t name in
    match Cudf.lookup_packages t.universe name with
    | [] ->
        [] (* Package not found *)
    | versions ->
        List.fast_sort (version_rev_compare t) versions (* Higher versions are preferred. *)
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

  let fresh_id {fresh_id; _} =
    incr fresh_id;
    !fresh_id
end

module Input = Model.Make(Context)

let requirements ~context pkgs =
  let role =
    let impl = Input.virtual_impl ~context ~depends:pkgs () in
    Input.virtual_role ~context [impl]
  in
  { Input.role; command = None }

module Solver = Zeroinstall_solver.Make(Input)
module Diagnostics = Zeroinstall_solver.Diagnostics(Solver.Output)

type t = Context.t
type selections = Solver.Output.t
type diagnostics = Input.requirements   (* So we can run another solve *)

let create ?(prefer_oldest=false) ?(handle_avoid_version=false) ~constraints universe =
  { universe; constraints; prefer_oldest; handle_avoid_version; fresh_id = ref 0 }

let solve context pkgs =
  let req = requirements ~context pkgs in
  match Solver.do_solve ~closest_match:false req with
  | Some sels -> Ok sels
  | None -> Error req

let diagnostics ?verbose req =
  Solver.do_solve req ~closest_match:true
  |> Option.get
  |> Diagnostics.get_failure_reason ?verbose

let packages_of_result sels =
  sels
  |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
  |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))
