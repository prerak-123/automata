type 'a nfa_letter = Single of 'a | Epsilon

val to_letter : 'a list -> 'a nfa_letter list
val from_letter : 'a nfa_letter list -> 'a list

module MakeSafe (Letter : Sig.Comparable) (State : Sig.Comparable) :
  Sig.NFA
    with type dfa_letter = Letter.t
     and type letter = Letter.t nfa_letter
     and type state = State.t
