module Make (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  type letter = Letter.t
  type state = State.t

  exception Invalid_DFA of string
  exception Invalid_letter
  exception Invalid_state

  module Alphabet_set = Set.Make (Letter)
  module State_set = Set.Make (State)

  type t = {
    states : State_set.t;
    alphabet : Alphabet_set.t;
    start : state;
    accepting : State_set.t;
    transitions : state -> letter -> state;
  }

  (* Auxilary Functions *)

  let check_states { states; start; accepting; _ } =
    match State_set.find_opt start states with
    | None -> false
    | Some _ -> State_set.subset accepting states

  let check_transitions { states; alphabet; transitions; _ } =
    let check_transitions_of_state state =
      Alphabet_set.for_all
        (fun a ->
          Option.is_some (State_set.find_opt (transitions state a) states))
        alphabet
    in
    State_set.for_all check_transitions_of_state states

  let rec step_aux dfa curr word =
    match word with
    | [] -> curr
    | hd :: tail -> step_aux dfa (dfa.transitions curr hd) tail

  (* Implementation of interface *)

  let create_exn ~alphabet ~states ~start ~accepting ~transitions =
    let dfa =
      {
        states = State_set.of_list states;
        alphabet = Alphabet_set.of_list alphabet;
        start;
        accepting = State_set.of_list accepting;
        transitions =
          (fun state letter ->
            if
              Option.is_none
                (Alphabet_set.find_opt letter (Alphabet_set.of_list alphabet))
            then raise Invalid_letter
            else transitions state letter);
      }
    in
    if Bool.not (check_states dfa) then raise (Invalid_DFA "Invalid State set")
    else if Bool.not (check_transitions dfa) then
      raise (Invalid_DFA "Invalid Transition Function")
    else dfa

  let step dfa ?start word =
    let start =
      match start with
      | None -> dfa.start
      | Some s ->
          if Option.is_none (State_set.find_opt s dfa.states) then
            raise Invalid_state
          else s
    in
    step_aux dfa start word

  let accept dfa word =
    let final = step_aux dfa dfa.start word in
    Option.is_some (State_set.find_opt final dfa.accepting)

  let state_product dfa1 dfa2 =
    let aux_add_pairs state1 curr =
      State_set.fold (fun state2 l -> (state1, state2) :: l) dfa2.states curr
    in
    State_set.fold (fun state1 l -> aux_add_pairs state1 l) dfa1.states []

  let negate dfa =
    { dfa with accepting = State_set.diff dfa.states dfa.accepting }
end
