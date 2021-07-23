module Make (C : S.CONTEXT) : sig
  include S.SOLVER with type t = C.t

  module Input : sig
    include Zeroinstall_solver.S.SOLVER_INPUT with type rejection = C.rejection
  end

  module Solver : sig
    module Output : Zeroinstall_solver.S.SOLVER_RESULT with module Input = Input
  end

  module Diagnostics : sig
    include module type of Zeroinstall_solver.Diagnostics(Solver.Output)
  end

  val diagnostics_rolemap : diagnostics -> Diagnostics.t
end
