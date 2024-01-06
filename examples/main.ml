open Automata
module A = Fsa.Make (Int) (Int)

let alphabet = [ 1; 2; 3 ]
let states = [ 1; 2; 3; 4 ]
let transitions state _ = state + 1
let fsa = A.create ~alphabet ~states ~transitions ~start:1 ~accepting:[ 1; 2 ]

let () =
  match fsa with Ok _ -> print_endline "OK!" | Error x -> print_endline x
