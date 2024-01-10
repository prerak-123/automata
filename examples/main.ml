open Automata
module DFA_INT = Dfa.MakeUnsafe (Int) (Int)
module DFA_STRING = Dfa.MakeUnsafe (Int) (String)
module Combine_INT_STRING = Combine.Make (DFA_INT) (DFA_STRING)
module DFA_INT_STRING = Combine_INT_STRING.F

let print_list l print_e =
  Printf.printf "[ ";
  let _ = List.map (fun e -> print_e e) l in
  Printf.printf "]\n";
  ()

let n = 300
let a = 50
let alphabet = List.init a (fun x -> x)
let states = List.init n (fun x -> x)
let transitions state _ = (state + 1) mod n

let dfa_int =
  DFA_INT.create_exn ~alphabet ~states ~transitions ~start:1 ~accepting:[ 0 ]

let reachable = DFA_INT.reachable dfa_int
let () = print_list reachable (fun x -> Printf.printf "%d " x)

let _ =
  DFA_INT.create_exn ~alphabet:[ 0; 1; 2 ] ~states:[ 0; 1; 2; 3 ]
    ~transitions:(fun _ x -> x)
    ~start:0 ~accepting:[ 2 ]

let dfa2 =
  DFA_STRING.create_exn ~alphabet ~states:[ "Cat"; "Dog" ]
    ~transitions:(fun x _ -> x)
    ~start:"Cat" ~accepting:[ "Dog" ]

let comb = Combine_INT_STRING.combine_or_exn dfa_int dfa2
let is_accepting = DFA_INT_STRING.accepts comb [ 0; 1 ]
let () = Printf.printf "Accepted: %b\n" is_accepting
let reachable = DFA_INT_STRING.reachable comb
let () = print_list reachable (fun (i, s) -> Printf.printf "(%d, %s) " i s)
