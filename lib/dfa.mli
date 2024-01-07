module Make (Letter : Sig.Comparable) (State : Sig.Comparable) :
  Sig.F with type letter = Letter.t and type state = State.t
