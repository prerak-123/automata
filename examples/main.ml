(* open Automata
   module DFA_Char = Dfa.MakeUnsafe (Int) (Char)

   let states = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]
   let alphabet = [ 0; 1 ]
   let start = 'a'
   let accepting = [ 'c'; 'd'; 'e' ]

   let transitions s a =
     match (s, a) with
     | 'a', 0 -> 'b'
     | 'b', 0 -> 'a'
     | 'c', 0 -> 'e'
     | 'd', 0 -> 'e'
     | 'e', 0 -> 'e'
     | 'f', 0 -> 'f'
     | 'a', 1 -> 'c'
     | 'b', 1 -> 'd'
     | 'c', 1 -> 'f'
     | 'd', 1 -> 'f'
     | 'e', 1 -> 'f'
     | 'f', 1 -> 'f'
     | _ -> 'x'

   let dfa = DFA_Char.create_exn ~states ~alphabet ~accepting ~start ~transitions
   let word = [ 0; 1; 0; 1 ]
   let min_dfa = DFA_Char.minimize dfa
   let accepts = DFA_Char.accepts dfa word
   let min_accepts = DFA_Char.accepts min_dfa word
   let states = DFA_Char.state_list dfa |> List.length
   let min_states = DFA_Char.state_list min_dfa |> List.length

   let () =
     Printf.printf "States in DFA: %d  |  States in minimal DFA : %d\n" states
       min_states

   let () =
     Printf.printf "Accepts: %b  |  Minimalized DFA Accepts : %b\n" accepts
       min_accepts *)

open Automata
module NFA_INT = Nfa.MakeSafe (Int) (Int)

let states = List.init 5 Fun.id
let alphabet = [ 0; 1; 2 ] |> Nfa.to_letter
let start = [ 0 ]
let accepting = [ 3 ]

let transitions s l =
  match l with Nfa.Epsilon -> [ s ] | Nfa.Single _ -> [ (s + 1) mod 5 ]

let nfa = NFA_INT.create_exn ~states ~alphabet ~start ~accepting ~transitions
let word = Nfa.to_letter [ 0; 1; 1; 1; 1; 1; 1; 2 ]
let next_states = NFA_INT.step nfa word

let () =
  let _ = List.map (fun x -> Printf.printf "%d " x) next_states in
  Printf.printf "\n"

let accepts = NFA_INT.accepts nfa word
let () = Printf.printf "%b\n" accepts
let reachable = NFA_INT.reachable nfa

let () =
  Printf.printf "Reachable States: ";
  let _ = List.map (fun x -> Printf.printf "%d " x) reachable in
  Printf.printf "\n"

let states = List.init 5 (fun x -> x + 5)
let start = [ 5 ]
let accepting = [ 7 ]

let transitions s l =
  match l with Nfa.Epsilon -> [ s ] | Nfa.Single _ -> [ ((s + 1) mod 5) + 5 ]

let nfa2 = NFA_INT.create_exn ~states ~alphabet ~start ~accepting ~transitions
let comb = NFA_INT.concatenate nfa nfa2
let word = word @ ([ 0; 0 ] |> Nfa.to_letter)
let accepts = NFA_INT.accepts comb word
let () = Printf.printf "%b\n" accepts
let nfa3 = NFA_INT.kleene_closure nfa (6, 7)
let word = List.init 14 (fun _ -> 0)
let next_states = NFA_INT.step nfa3 (Nfa.to_letter word)

let () =
  let _ = List.map (fun x -> Printf.printf "%d " x) next_states in
  Printf.printf "\n"

let accepts = NFA_INT.accepts nfa3 (Nfa.to_letter word)
let () = Printf.printf "Accepts: %b\n" accepts

let nfa4 = NFA_INT.reverse nfa
let word = List.init 2 (fun _ -> 0)
let accepts = NFA_INT.accepts nfa4 (Nfa.to_letter word)

let () = Printf.printf "Accepts %b\n" accepts
