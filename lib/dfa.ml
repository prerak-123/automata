module MakeSafe (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  type letter = Letter.t
  type state = State.t

  let state_compare = State.compare
  let letter_compare = Letter.compare

  exception Invalid_DFA of string
  exception Invalid_letter
  exception Invalid_state

  module AlphabetSet = Set.Make (Letter)
  module StateSet = Set.Make (State)

  type t = {
    states : StateSet.t;
    alphabet : AlphabetSet.t;
    start : state;
    accepting : StateSet.t;
    transitions : state -> letter -> state;
  }

  (* Auxilary Functions *)

  let check_states { states; start; accepting; _ } =
    match StateSet.find_opt start states with
    | None -> false
    | Some _ -> StateSet.subset accepting states

  let check_transitions { states; alphabet; transitions; _ } =
    let check_transitions_of_state state =
      AlphabetSet.for_all
        (fun a ->
          Option.is_some (StateSet.find_opt (transitions state a) states))
        alphabet
    in
    StateSet.for_all check_transitions_of_state states

  let rec step_aux dfa curr word =
    match word with
    | [] -> curr
    | hd :: tail -> step_aux dfa (dfa.transitions curr hd) tail

  let rec reach_aux visited curr transitions alphabet_list =
    let new_visited = StateSet.add curr visited in
    List.fold_left
      (fun v a ->
        let next_state = transitions curr a in
        match StateSet.find_opt next_state new_visited with
        | Some _ -> v
        | None -> reach_aux v next_state transitions alphabet_list)
      new_visited alphabet_list

  (* Implementation of interface *)

  let alphabet_list { alphabet; _ } = AlphabetSet.to_list alphabet
  let state_list { states; _ } = StateSet.to_list states
  let start { start; _ } = start
  let accepting_list { accepting; _ } = StateSet.to_list accepting
  let transitions { transitions; _ } = transitions

  let create_exn ~alphabet ~states ~start ~accepting ~transitions =
    let dfa =
      {
        states = StateSet.of_list states;
        alphabet = AlphabetSet.of_list alphabet;
        start;
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
    if Bool.not (check_states dfa) then raise (Invalid_DFA "Invalid State set")
    else if Bool.not (check_transitions dfa) then
      raise (Invalid_DFA "Invalid Transition Function")
    else dfa

  let step dfa ?start word =
    let start =
      match start with
      | None -> dfa.start
      | Some s ->
          if Option.is_none (StateSet.find_opt s dfa.states) then
            raise Invalid_state
          else s
    in
    step_aux dfa start word

  let accepts dfa word =
    let final = step_aux dfa dfa.start word in
    Option.is_some (StateSet.find_opt final dfa.accepting)

  let negate dfa =
    { dfa with accepting = StateSet.diff dfa.states dfa.accepting }

  let reachable dfa =
    StateSet.to_list
      (reach_aux StateSet.empty dfa.start dfa.transitions (alphabet_list dfa))
end

module MakeUnsafe (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  (* type letter = Letter.t
     type state = State.t

     let state_compare = State.compare
     let letter_compare = Letter.compare

     exception Invalid_DFA of string
     exception Invalid_letter
     exception Invalid_state

     module AlphabetSet = Set.Make (Letter)
     module StateSet = Set.Make (State)

     type t = {
       states : StateSet.t;
       alphabet : AlphabetSet.t;
       start : state;
       accepting : StateSet.t;
       transitions : state -> letter -> state;
     }

     (* Auxilary Functions *)

     let rec step_aux dfa curr word =
       match word with
       | [] -> curr
       | hd :: tail -> step_aux dfa (dfa.transitions curr hd) tail

     let rec reach_aux visited curr transitions alphabet_list =
       let new_visited = StateSet.add curr visited in
       List.fold_left
         (fun v a ->
           let next_state = transitions curr a in
           match StateSet.find_opt next_state new_visited with
           | Some _ -> v
           | None -> reach_aux v next_state transitions alphabet_list)
         new_visited alphabet_list

     (* Implementation of interface *)

     let alphabet_list { alphabet; _ } = AlphabetSet.to_list alphabet
     let state_list { states; _ } = StateSet.to_list states
     let start { start; _ } = start
     let accepting_list { accepting; _ } = StateSet.to_list accepting
     let transitions { transitions; _ } = transitions

     let create_exn ~alphabet ~states ~start ~accepting ~transitions =
         {
           states = StateSet.of_list states;
           alphabet = AlphabetSet.of_list alphabet;
           start;
           accepting = StateSet.of_list accepting;
           transitions = transitions
         }

     let step dfa ?start word =
       let start =
         match start with
         | None -> dfa.start
         | Some s -> s
       in
       step_aux dfa start word

     let accepts dfa word =
       let final = step_aux dfa dfa.start word in
       Option.is_some (StateSet.find_opt final dfa.accepting)

     let negate dfa =
       { dfa with accepting = StateSet.diff dfa.states dfa.accepting }

     let reachable dfa =
       StateSet.to_list
         (reach_aux StateSet.empty dfa.start dfa.transitions (alphabet_list dfa)) *)
  module T = MakeSafe (Letter) (State)
  include T

  let create_exn ~alphabet ~states ~start ~accepting ~transitions =
    {
      states = StateSet.of_list states;
      alphabet = AlphabetSet.of_list alphabet;
      start;
      accepting = StateSet.of_list accepting;
      transitions;
    }

  let step dfa ?start word =
    let start = match start with None -> dfa.start | Some s -> s in
    step_aux dfa start word
end
