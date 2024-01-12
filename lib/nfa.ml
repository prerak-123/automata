type 'a nfa_letter = Single of 'a | Epsilon

let to_letter l = Epsilon :: List.map (fun x -> Single x) l

let from_letter l =
  List.filter_map
    (fun x -> match x with Epsilon -> None | Single x -> Some x)
    l

module MakeSafe (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  exception Invalid_NFA of string
  exception Invalid_letter
  exception Invalid_state

  type letter = Letter.t nfa_letter
  type state = State.t

  module LetterCompare = struct
    type t = letter

    let compare l1 l2 =
      match (l1, l2) with
      | Epsilon, Epsilon -> 0
      | Epsilon, Single _ -> -1
      | Single _, Epsilon -> 1
      | Single c1, Single c2 -> Letter.compare c1 c2
  end

  module AlphabetSet = Set.Make (LetterCompare)
  module StateSet = Set.Make (State)

  type t = {
    states : StateSet.t;
    alphabet : AlphabetSet.t;
    start : StateSet.t;
    accepting : StateSet.t;
    transitions : state -> letter -> state list;
  }

  (* Auxillary Functions *)

  let check_states nfa =
    if Bool.not (StateSet.subset nfa.start nfa.states) then false
    else StateSet.subset nfa.accepting nfa.states

  let check_transitions nfa =
    let check_transitions_of_state state =
      AlphabetSet.for_all
        (fun a ->
          let next_states = nfa.transitions state a |> StateSet.of_list in
          StateSet.subset next_states nfa.states)
        nfa.alphabet
    in
    StateSet.for_all check_transitions_of_state nfa.states

  (* Interface Implementation *)

  let create_exn ~alphabet ~states ~start ~accepting ~transitions =
    let nfa =
      {
        states = StateSet.of_list states;
        alphabet = AlphabetSet.of_list alphabet |> AlphabetSet.add Epsilon;
        start = StateSet.of_list start;
        accepting = StateSet.of_list accepting;
        transitions;
      }
    in
    if Bool.not (check_states nfa) then raise (Invalid_NFA "Invalid State set")
    else if Bool.not (check_transitions nfa) then
      raise (Invalid_NFA "Invalid Transition Function")
    else nfa
end
