type 'a nfa_letter = Single of 'a | Epsilon

let to_letter l = List.map (fun x -> Single x) l

let from_letter l =
  List.filter_map (function Epsilon -> None | Single x -> Some x) l

module MakeSafe (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  exception Invalid_NFA of string
  exception Invalid_letter
  exception Invalid_states
  exception Alphabets_not_equal

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

  let strip_word word =
    List.filter (function Single _ -> true | Epsilon -> false) word

  let next_states nfa curr_states a =
    StateSet.fold
      (fun s next_states ->
        nfa.transitions s a |> StateSet.of_list |> StateSet.union next_states)
      curr_states StateSet.empty

  let rec epsilon_closure nfa states =
    let ns = next_states nfa states Epsilon in
    if StateSet.subset ns states then states
    else epsilon_closure nfa (StateSet.union ns states)

  let single_step nfa states a = next_states nfa states a |> epsilon_closure nfa

  let check_states nfa =
    if not (StateSet.subset nfa.start nfa.states) then false
    else StateSet.subset nfa.accepting nfa.states

  let check_transitions nfa =
    let check_transitions_of_state state =
      AlphabetSet.for_all
        (fun a ->
          let ns = next_states nfa (StateSet.singleton state) a in
          StateSet.subset ns nfa.states)
        nfa.alphabet
    in
    StateSet.for_all check_transitions_of_state nfa.states

  let rec aux_reachable nfa curr_states visited =
    if StateSet.subset curr_states visited then visited
    else
      let new_visited = StateSet.union curr_states visited in
      AlphabetSet.fold
        (fun a v ->
          let next_states = single_step nfa curr_states a in
          aux_reachable nfa next_states v)
        nfa.alphabet new_visited

  (* Interface Implementation *)

  let create_exn ~alphabet ~states ~start ~accepting ~transitions =
    let alphabet = Epsilon :: alphabet in
    let nfa =
      {
        states = StateSet.of_list states;
        alphabet = AlphabetSet.of_list alphabet;
        start = StateSet.of_list start;
        accepting = StateSet.of_list accepting;
        transitions =
          (fun state letter ->
            if
              Option.is_none
                (AlphabetSet.find_opt letter (AlphabetSet.of_list alphabet))
            then raise Invalid_letter
            else transitions state letter);
      }
    in
    if not (check_states nfa) then raise (Invalid_NFA "Invalid State set")
    else if not (check_transitions nfa) then
      raise (Invalid_NFA "Invalid Transition Function")
    else nfa

  let alphabet_list { alphabet; _ } = AlphabetSet.to_list alphabet
  let state_list { states; _ } = StateSet.to_list states
  let start_list { start; _ } = StateSet.to_list start
  let accepting_list { accepting; _ } = StateSet.to_list accepting
  let transitions { transitions; _ } = transitions

  let step nfa ?start word =
    let start =
      match start with
      | None -> nfa.start
      | Some s ->
          let start_set = StateSet.of_list s in
          if not (StateSet.subset start_set nfa.states) then
            raise Invalid_states
          else start_set
    in
    let start = epsilon_closure nfa start in
    strip_word word
    |> List.fold_left (single_step nfa) start
    |> StateSet.to_list

  let accepts nfa word =
    let final_states =
      strip_word word
      |> List.fold_left (single_step nfa) (epsilon_closure nfa nfa.start)
    in
    not (StateSet.is_empty (StateSet.inter final_states nfa.accepting))

  let reachable nfa =
    aux_reachable nfa nfa.start StateSet.empty |> StateSet.to_list

  let concatenate nfa1 nfa2 =
    if not (StateSet.inter nfa1.states nfa2.states |> StateSet.is_empty) then
      raise Invalid_states
    else if not (AlphabetSet.equal nfa1.alphabet nfa2.alphabet) then
      raise Alphabets_not_equal
    else
      let new_transition state = function
        | Single _ as x ->
            if StateSet.find_opt state nfa1.states |> Option.is_some then
              nfa1.transitions state x
            else nfa2.transitions state x
        | Epsilon as x ->
            if StateSet.find_opt state nfa1.accepting |> Option.is_some then
              StateSet.to_list nfa2.start @ nfa1.transitions state x
            else if StateSet.find_opt state nfa1.states |> Option.is_some then
              nfa1.transitions state x
            else nfa2.transitions state x
      in
      {
        alphabet = nfa1.alphabet;
        states = StateSet.union nfa1.states nfa2.states;
        start = nfa1.start;
        accepting = nfa2.accepting;
        transitions = new_transition;
      }

  let kleene_closure nfa (new_start, new_end) =
    if
      Option.is_some (StateSet.find_opt new_start nfa.states)
      || Option.is_some (StateSet.find_opt new_end nfa.states)
    then raise Invalid_states
    else
      let new_transition state letter =
        if State.compare state new_start = 0 then
          match letter with
          | Epsilon -> StateSet.add new_end nfa.start |> StateSet.to_list
          | Single _ -> []
        else if State.compare state new_end = 0 then []
        else if StateSet.find_opt state nfa.accepting |> Option.is_some then
          match letter with
          | Single _ -> nfa.transitions state letter
          | Epsilon ->
              nfa.transitions state letter
              |> StateSet.of_list |> StateSet.union nfa.start
              |> StateSet.add new_end |> StateSet.to_list
        else nfa.transitions state letter
      in
      {
        alphabet = nfa.alphabet;
        states = nfa.states |> StateSet.add new_start |> StateSet.add new_end;
        start = StateSet.singleton new_start;
        accepting = StateSet.singleton new_end;
        transitions = new_transition;
      }
end
