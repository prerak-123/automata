open Automata
module DFA_INT = Dfa.MakeUnsafe (Int) (Int)
(* module DFA_STRING = Dfa.MakeUnsafe (Int) (String)
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
   let transitions state letter = ((state * letter) + state + letter) mod n

   let dfa_int =
     DFA_INT.create_exn ~alphabet ~states ~transitions ~start:1 ~accepting:[ 0 ]

   let reachable = DFA_INT.reachable dfa_int
   let () = print_list reachable (fun x -> Printf.printf "%d " x)

   let _ =
     DFA_INT.create_exn ~alphabet:[ 0; 1; 2 ] ~states:[ 0; 1; 2; 3 ]
       ~transitions:(fun _ x -> x)
       ~start:0 ~accepting:[ 2 ]

   let dfa2 =
     DFA_STRING.create_exn ~alphabet
       ~states:[ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]
       ~transitions:(fun x _ ->
         if x = "a" then "b" else if x = "b" then "c" else "g")
       ~start:"a" ~accepting:[ "a" ]

   let comb =
     DFA_INT_STRING.negate (Combine_INT_STRING.combine_or_exn dfa_int dfa2)

   let is_accepting = DFA_INT_STRING.accepts comb [ 0; 1 ]
   let () = Printf.printf "Accepted: %b\n" is_accepting
   let reachable = DFA_INT_STRING.reachable comb
   let () = print_list reachable (fun (i, s) -> Printf.printf "(%d, %s) " i s) *)

let n1 = 10000
let states = List.init n1 (fun x -> x)
let alphabet = [ 0; 1; 2; 3 ]
let n2 = 20
let accepting = List.init (n1 / n2) (fun x -> n2 * x)

let dfa =
  DFA_INT.create_exn ~states ~alphabet
    ~transitions:(fun x _ -> (x + 1) mod n1)
    ~start:0 ~accepting

let min_dfa = DFA_INT.minimize dfa
let word_len = 2000000
let word = List.init word_len (fun _ -> 0)
let accepts = DFA_INT.accepts dfa word
let min_accepts = DFA_INT.accepts min_dfa word
let states = DFA_INT.state_list dfa |> List.length
let min_states = DFA_INT.state_list min_dfa |> List.length

let () =
  Printf.printf "States in DFA: %d  |  States in minimal DFA : %d\n" states
    min_states

let () =
  Printf.printf "Accepts: %b  | Minimalized DFA Accepts : %b\n" accepts
    min_accepts
