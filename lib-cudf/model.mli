(** This module maps between the opam and 0install concepts. Roughly:

    - An opam package name is a 0install role.
    - An opam package is a 0install implementation.
    - An opam version formula is a 0install restriction.

    For dependencies:

    - depends become "essential" dependencies
    - depopts are ignored (the opam solver ignores them too; they don't have constraints)
    - conflicts become "restricts" (with the test reversed)

    Dependencies on alternatives (e.g. "ocaml-base-compiler | ocaml-variants")
    become a dependency on a virtual package which has each choice as an
    implementation. *)

val fop : Cudf_types.relop -> int -> int -> bool

module Make (Context : S.CONTEXT) : sig
  type restriction = {
    kind : [ `Ensure | `Prevent ];
    expr : (Cudf_types.relop * Cudf_types.version) list; (* TODO: might not be a list *)
    (* NOTE: each list is a raw or the list is an OR case (see Cudf_types.vpkgforula) *)
  }

  type real_role = {
    context : Context.t;
    name : Cudf_types.pkgname;
  }

  type role =
    | Real of real_role               (* A role is usually an opam package name *)
    | Virtual of int * impl list      (* (int just for sorting) *)
  and real_impl = {
    pkg : Cudf.package;
    requires : dependency list;
  }
  and dependency = {
    drole : role;
    importance : [ `Essential | `Recommended | `Restricts ];
    restrictions : restriction list;
  }
  and impl =
    | RealImpl of real_impl                     (* An implementation is usually an opam package *)
    | VirtualImpl of int * dependency list      (* (int just for sorting) *)
    | Reject of (Cudf_types.pkgname * Cudf_types.version)
    | Dummy                                     (* Used for diagnostics *)

  module Role : sig
    type t = role
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end

  include Zeroinstall_solver.S.SOLVER_INPUT
    with type restriction := restriction
     and type impl := impl
     and type dependency := dependency
     and type rejection = Context.rejection
     and module Role := Role

  val role : Context.t -> Cudf_types.pkgname -> Role.t

  val version : impl -> (Cudf_types.pkgname * Cudf_types.version) option
  (** [version impl] is the Opam package for [impl], if any.
      Virtual and dummy implementations return [None]. *)

  val virtual_role : impl list -> Role.t
  (** [virtual_role impls] is a virtual package name with candidates [impls].
      This is used if the user requests multiple packages on the command line
      (the single [impl] will also be virtual). *)

  val virtual_impl :
    context:Context.t ->
    depends:(Cudf_types.pkgname * [`Essential | `Recommended]) list ->
    unit ->
    impl
  (** [virtual_impl ~context ~depends] is a virtual package which just depends
      on [depends]. This is used if the user requests multiple packages on the
      command line - each requested package becomes a dependency of the virtual
      implementation according to their associated requirement tag. *)
end
