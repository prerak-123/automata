module Make (Alphabet : Sig.Comparable) (State : Sig.Comparable) = struct
  type alphabet = Alphabet.t
  type state = State.t

  module Alphabet_set = Set.Make (Alphabet)
  module State_set = Set.Make (State)

  type t = {
    states : State_set.t;
    alphabet : Alphabet_set.t;
    start : state;
    accepting : State_set.t;
    transitions : state -> alphabet -> state;
  }

  (* Auxilary Functions *)

  let check_states { states; start; accepting; _ } =
    match State_set.find_opt start states with
    | None -> false
    | Some _ -> State_set.subset accepting states

  let check_transitions { states; alphabet; transitions; _ } =
    let alphabet_list = Alphabet_set.to_list alphabet
    and state_list = State_set.to_list states in

    let check_transitions_of_state state =
      List.for_all
        (fun a ->
          Option.is_some (State_set.find_opt (transitions state a) states))
        alphabet_list
    in
    List.for_all check_transitions_of_state state_list

  (* Implementation of interface *)

  let create ~alphabet ~states ~start ~accepting ~transitions =
    let x =
      {
        states = State_set.of_list states;
        alphabet = Alphabet_set.of_list alphabet;
        start;
        accepting = State_set.of_list accepting;
        transitions;
      }
    in
    if Bool.not (check_states x) then Error "Invalid State set"
    else if Bool.not (check_transitions x) then
      Error "Invalid Transition Function"
    else Ok x
end
