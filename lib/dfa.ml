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
  module StatePartition = Set.Make (StateSet)

  type t = {
    states : StateSet.t;
    alphabet : AlphabetSet.t;
    start : state;
    accepting : StateSet.t;
    transitions : state -> letter -> state;
  }

  let alphabet_list { alphabet; _ } = AlphabetSet.to_list alphabet
  let state_list { states; _ } = StateSet.to_list states
  let start { start; _ } = start
  let accepting_list { accepting; _ } = StateSet.to_list accepting
  let transitions { transitions; _ } = transitions

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

  let partition dfa =
    let reachable =
      reach_aux StateSet.empty dfa.start dfa.transitions (alphabet_list dfa)
    in
    let final = StateSet.inter reachable dfa.accepting in
    if StateSet.cardinal final = 0 then StatePartition.singleton reachable
    else
      let get_inverse s a =
        StateSet.fold
          (fun ss l ->
            let next_state = dfa.transitions ss a in
            if Option.is_some (StateSet.find_opt next_state s) then
              StateSet.add ss l
            else l)
          reachable StateSet.empty
      in
      let rec refine p w =
        if StatePartition.cardinal w = 0 then p
        else
          let s = StatePartition.choose w in
          let w = StatePartition.remove s w in
          let p, w =
            AlphabetSet.fold
              (fun letter (p, w) ->
                let la = get_inverse s letter in
                StatePartition.fold
                  (fun r (p, w) ->
                    if
                      StateSet.equal StateSet.empty (StateSet.inter r la)
                      || StateSet.subset r la
                    then (p, w)
                    else
                      let r1 = StateSet.inter r la in
                      let r2 = StateSet.diff r r1 in
                      let p =
                        StatePartition.remove r p |> StatePartition.add r1
                        |> StatePartition.add r2
                      in
                      if Option.is_some (StatePartition.find_opt r w) then
                        let w =
                          StatePartition.remove r w |> StatePartition.add r2
                          |> StatePartition.add r2
                        in
                        (p, w)
                      else
                        let w =
                          if StateSet.cardinal r1 <= StateSet.cardinal r2 then
                            StatePartition.add r1 w
                          else StatePartition.add r2 w
                        in
                        (p, w))
                  p (p, w))
              dfa.alphabet (p, w)
          in
          refine p w
      in
      let remain = StateSet.diff reachable final in
      if StateSet.cardinal remain = 0 then StatePartition.singleton final
      else
        let w = StatePartition.of_list [ remain; final ] in
        let p = w in
        refine p w

  (* Implementation of interface *)

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
    if not (check_states dfa) then raise (Invalid_DFA "Invalid State set")
    else if not (check_transitions dfa) then
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

  let minimize dfa =
    let partitions = StatePartition.to_list (partition dfa) in
    let states =
      List.map (fun x -> StateSet.choose x) partitions |> StateSet.of_list
    in
    let start =
      StateSet.choose
        (List.find
           (fun x -> Option.is_some (StateSet.find_opt dfa.start x))
           partitions)
    in
    let accepting =
      List.filter (fun x -> StateSet.subset x dfa.accepting) partitions
      |> List.map (fun x -> StateSet.choose x)
      |> StateSet.of_list
    in
    let transitions s a =
      let next_state = dfa.transitions s a in
      List.find
        (fun x -> Option.is_some (StateSet.find_opt next_state x))
        partitions
      |> StateSet.choose
    in
    { dfa with states; start; accepting; transitions }
end

module MakeUnsafe (Letter : Sig.Comparable) (State : Sig.Comparable) = struct
  include MakeSafe (Letter) (State)

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
