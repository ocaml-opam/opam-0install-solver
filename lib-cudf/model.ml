let fop : Cudf_types.relop -> int -> int -> bool = function
  | `Eq -> (=)
  | `Neq -> (<>)
  | `Geq -> (>=)
  | `Gt -> (>)
  | `Leq -> (<=)
  | `Lt -> (<)

module Make (Context : S.CONTEXT) = struct
  type restriction = {
    kind : [ `Ensure | `Prevent ];
    expr : (Cudf_types.relop * Cudf_types.version) list; (* TODO: might not be a list *)
    (* NOTE: each list is a raw or the list is an OR case (see Cudf_types.vpkgforula) *)
  }

  type dependency = {
    drole : role;
    importance : [ `Essential | `Recommended | `Restricts ];
    restrictions : restriction list;
  }
  and impl =
    | RealImpl of {pkg : Cudf.package; requires : dependency list}
    | VirtualImpl of int * dependency list
    | Dummy
  and role =
    | Real of {context : Context.t; name : Cudf_types.pkgname}
    | Virtual of int * impl list

  let rec pp_version fmt = function
    | RealImpl impl -> Fmt.int fmt impl.pkg.Cudf.version
    | VirtualImpl (_, deps) -> Fmt.string fmt (String.concat "&" (List.map (fun d -> Fmt.to_to_string pp_role d.drole) deps))
    | Dummy -> Fmt.string fmt "(no version)"
  and pp_impl fmt = function
    | RealImpl impl -> Fmt.string fmt impl.pkg.Cudf.package
    | VirtualImpl _ as x -> pp_version fmt x
    | Dummy -> Fmt.string fmt "(no solution found)"
  and pp_role fmt = function
    | Real t -> Fmt.string fmt t.name
    | Virtual (_, impls) -> Fmt.pf fmt "%a" Fmt.(list ~sep:(unit "|") pp_impl) impls

  let pp_impl_long fmt = function
    | RealImpl impl -> Fmt.pf fmt "%s.%d" impl.pkg.Cudf.package impl.pkg.Cudf.version
    | VirtualImpl _ as x -> pp_version fmt x
    | Dummy -> Fmt.string fmt "(no solution found)"

  module Role = struct
    type t = role

    let pp fmt = function
      | Real t -> Fmt.string fmt t.name
      | Virtual (_, impls) -> Fmt.pf fmt "%a" Fmt.(list ~sep:(unit "|") pp_impl) impls

    let compare a b =
      match a, b with
      | Real t, Real t' -> String.compare t.name t'.name
      | Virtual (a, _), Virtual (b, _) -> compare (a : int) b
      | Real _, Virtual _ -> -1
      | Virtual _, Real _ -> 1
  end

  type command = |
  type command_name = private string
  let pp_command _ = function (_:command) -> .
  let command_requires _role = function (_:command) -> .
  let get_command _impl _command_name = None

  type dep_info = {
    dep_role : Role.t;
    dep_importance : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role : Role.t;
    command : command_name option;
  }

  type role_information = {
    replacement : Role.t option;
    impls : impl list;
  }

  type machine_group = private string
  let machine_group _impl = None

  type conflict_class = private string
  let conflict_class _impl = []

  type rejection = Context.rejection

  let requires _ = function
    | RealImpl impl -> (impl.requires, [])
    | VirtualImpl (_, deps) -> (deps, [])
    | Dummy -> ([], [])

  let dep_info dep = {
    dep_role = dep.drole;
    dep_importance = dep.importance;
    dep_required_commands = [];
  }

  let fresh_id =
    let i = ref 0 in
    fun () ->
      incr i;
      !i

  let role context name = Real {context; name}
  let virtual_role impls = Virtual (fresh_id (), impls)

  let list_deps ~context ~importance ~kind deps =
    let rec aux = function
      | [[(name, constr)]] ->
          let drole = role context name in
          let restrictions =
            match constr with
            | None -> []
            | Some c -> [{kind; expr = [c]}]
          in
          [{ drole; restrictions; importance }]
      | [o] ->
          let impls = group_ors o in
          let drole = virtual_role impls in
          (* Essential because we must apply a restriction, even if its
             components are only restrictions. *)
          [{ drole; restrictions = []; importance = `Essential }]
      | x::y -> aux [x] @ aux y
      | [] -> []
    and group_ors = function
      | [expr] -> [VirtualImpl (fresh_id (), aux [[expr]])]
      | x::y -> group_ors [x] @ group_ors y
      | [] -> assert false (* TODO: implement false *)
    in
    aux deps

  let ensure l = l

  let prevent l = List.map (fun x -> [x]) l

  let implementations = function
    | Real role ->
        let impls =
          Context.candidates role.context role.name
          |> List.filter_map (function
            | _, Some _rejection -> None
            | version, None ->
                let pkg = Context.load role.context (role.name, version) in
                let requires =
                  let make_deps importance kind deps =
                    list_deps ~context:role.context ~importance ~kind deps
                  in
                  make_deps `Essential `Ensure (ensure pkg.Cudf.depends) @
                  make_deps `Restricts `Prevent (prevent pkg.Cudf.conflicts)
                in
                Some (RealImpl {pkg; requires})
          )
        in
        {replacement = None; impls}
    | Virtual (_, impls) ->
        {replacement = None; impls}

  let restrictions dep =
    dep.restrictions

  let meets_restriction impl {kind; expr} =
    match impl with
    | Dummy -> true
    | VirtualImpl _ -> assert false
    | RealImpl impl ->
        let aux (c, v) = fop c impl.pkg.Cudf.version v in
        let res = List.exists aux expr in
        match kind with
        | `Ensure -> res
        | `Prevent -> not res

  let rejects = function
    | Virtual _ -> ([], [])
    | Real role ->
        let rejects =
          Context.candidates role.context role.name
          |> List.filter_map (function
            | _, None -> None
            | version, Some reason ->
                let pkg = Context.load role.context (role.name, version) in
                Some (RealImpl {pkg; requires = []}, reason)
          )
        in
        (rejects, [])

  let compare_version a b =
    match a, b with
    | RealImpl a, RealImpl b -> compare (a.pkg.Cudf.version : int) b.pkg.Cudf.version
    | VirtualImpl (ia, _), VirtualImpl (ib, _) -> compare (ia : int) ib
    | _, _ -> compare a b (* TODO: do better *)

  let user_restrictions = function
    | Virtual _ -> None
    | Real role ->
        match Context.user_restrictions role.context role.name with
        | [] -> None
        | expr -> Some { kind = `Ensure; expr }

  let format_machine _impl = "(src)"

  let string_of_op = function
    | `Eq -> "="
    | `Geq -> ">="
    | `Gt -> ">"
    | `Leq -> "<="
    | `Lt -> "<"
    | `Neq -> "<>"

  let string_of_version_formula l =
    String.concat " & " (
      List.map (fun (rel, v) ->
        Printf.sprintf "%s %s" (string_of_op rel) (string_of_int v)
      ) l
    )

  let string_of_restriction = function
    | { kind = `Prevent; expr = [] } -> "conflict with all versions"
    | { kind = `Prevent; expr } -> Fmt.strf "not(%s)" (string_of_version_formula expr)
    | { kind = `Ensure; expr } -> string_of_version_formula expr

  let describe_problem _impl = Fmt.to_to_string Context.pp_rejection

  let dummy_impl = Dummy

  let version = function
    | RealImpl impl -> Some (impl.pkg.Cudf.package, impl.pkg.Cudf.version)
    | VirtualImpl _ -> None
    | Dummy -> None

  let virtual_impl ~context ~depends () =
    let depends =
      List.map (fun name ->
        {drole = role context name; importance = `Essential; restrictions = []}
      ) depends
    in
    VirtualImpl (fresh_id (), depends)
end
