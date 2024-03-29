module MakeSafe (Letter : Sig.Comparable) (State : Sig.Comparable) :
  Sig.DFA with type letter = Letter.t and type state = State.t

module MakeUnsafe (Letter : Sig.Comparable) (State : Sig.Comparable) :
  Sig.DFA with type letter = Letter.t and type state = State.t
