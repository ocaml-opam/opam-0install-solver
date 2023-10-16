module Switch_context : functor (M : S.MONAD) -> functor (C : S.CONTEXT) -> sig
  include module type of C(M)

  val create :
    ?prefer_oldest:bool ->
    ?test:OpamPackage.Name.Set.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    OpamStateTypes.unlocked OpamStateTypes.switch_state ->
    t
  (** [create ~constraints switch] is a solver that gets candidates from [switch], filtering them
      using [constraints].

      @param test Packages for which we should include "with-test" dependencies.

      @param prefer_oldest if [true] the solver is set to return the least
      up-to-date version of each package, if a solution exists. This is [false] by
      default.
      @before 0.4 the [prefer_oldest] parameter did not exist. *)
end
