open Automata
module DFA_INT = Dfa.Make (Int) (Int)
module DFA_STRING = Dfa.Make (Int) (String)
module Combine_INT_STRING = Combine.Make (DFA_INT) (DFA_STRING)
module DFA_INT_STRING = Combine_INT_STRING.F

let n = 3000
let a = 50
let alphabet = List.init a (fun x -> x)
let states = List.init n (fun x -> x)
let transitions state letter = (state + letter) mod n

let dfa_int =
  DFA_INT.create_exn ~alphabet ~states ~transitions ~start:0 ~accepting:[ 0 ]

let l = 1000007
let word = List.init l (fun x -> x mod a)
let final = DFA_INT.step dfa_int word
let accept = DFA_INT.accept dfa_int word
let () = Printf.printf "Final State = %n Accepting: %b\n" final accept
let dfa_not = DFA_INT.negate dfa_int
let final = DFA_INT.step dfa_not word
let accept = DFA_INT.accept dfa_not word
let () = Printf.printf "Final State = %n Accepting: %b\n" final accept

let dfa1 =
  DFA_INT.create_exn ~alphabet:[ 0; 1; 2 ] ~states:[ 0; 1; 2 ]
    ~transitions:(fun _ x -> x)
    ~start:0 ~accepting:[ 2 ]

let dfa2 =
  DFA_STRING.create_exn ~alphabet:[ 0; 1; 2 ] ~states:[ "Cat"; "Dog" ]
    ~transitions:(fun x _ -> x)
    ~start:"Cat" ~accepting:[ "Cat" ]

let comb = Combine_INT_STRING.combine_and_exn dfa1 dfa2

let is_accepting = DFA_INT_STRING.accept comb [2]

let () = Printf.printf "Accepted: %b\n" is_accepting