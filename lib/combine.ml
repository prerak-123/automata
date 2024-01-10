module Make (F1 : Sig.F) (F2 : Sig.F with type letter = F1.letter) = struct
  type letter = F1.letter
  type state1 = F1.state
  type state2 = F2.state

  exception Alphabets_not_matching

  module Letter = struct
    type t = letter

    let compare = F1.letter_compare
  end

  module New_state = struct
    type t = state1 * state2

    let compare (s1, s2) (ns1, ns2) =
      if F1.state_compare s1 ns1 = 0 then F2.state_compare s2 ns2
      else F1.state_compare s1 ns1
  end

  module F = Dfa.MakeUnsafe (Letter) (New_state)

  (* Auxilary Functions *)

  let state_product states1 states2 =
    let add_pairs curr state =
      List.fold_left (fun l s -> (state, s) :: l) curr states2
    in
    List.fold_left add_pairs [] states1

  let equality_alphabets alphabet1 alphabet2 =
    let alphabet1 = List.sort Letter.compare alphabet1
    and alphabet2 = List.sort Letter.compare alphabet2 in
    List.equal (fun x y -> Letter.compare x y = 0) alphabet1 alphabet2

  (* Interface Implemantation *)

  let combine_and_exn dfa1 dfa2 =
    if
      Bool.not
        (equality_alphabets (F1.alphabet_list dfa1) (F2.alphabet_list dfa2))
    then raise Alphabets_not_matching
    else
      let new_states =
        state_product (F1.state_list dfa1) (F2.state_list dfa2)
      in
      let new_start = (F1.start dfa1, F2.start dfa2) in
      let new_alphabet = F1.alphabet_list dfa1 in
      let new_accepting =
        state_product (F1.accepting_list dfa1) (F2.accepting_list dfa2)
      in
      let new_transitions (s1, s2) letter =
        (F1.transitions dfa1 s1 letter, F2.transitions dfa2 s2 letter)
      in
      F.create_exn ~alphabet:new_alphabet ~states:new_states ~start:new_start
        ~accepting:new_accepting ~transitions:new_transitions

  let combine_or_exn dfa1 dfa2 =
    let not_dfa1 = F1.negate dfa1 and not_dfa2 = F2.negate dfa2 in
    let comb = combine_and_exn not_dfa1 not_dfa2 in
    F.negate comb
end
