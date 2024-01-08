module Make (F1 : Sig.F) (F2 : Sig.F with type letter = F1.letter) : sig
  type letter = F1.letter
  type state1 = F1.state
  type state2 = F2.state

  exception Alphabets_not_matching

  module F : Sig.F with type letter = letter and type state = state1 * state2

  val combine_and_exn : F1.t -> F2.t -> F.t
  val combine_or_exn : F1.t -> F2.t -> F.t
end
