module Make (Alphabet : Sig.Comparable) (State : Sig.Comparable) :
  Sig.F with type alphabet = Alphabet.t and type state = State.t
