open Automata
module A = Dfa.Make (Int) (Int)

let n1 = 5
and n2 = 5

let a = 10
let alphabet = List.init a (fun x -> x)
let states1 = List.init n1 (fun x -> x)
let states2 = List.init n2 (fun x -> x)
let transitions1 state letter = (state + letter) mod n1
let transitions2 state letter = (state + letter) mod n2

let dfa1 =
  A.create_exn ~alphabet ~states:states1 ~transitions:transitions1 ~start:0
    ~accepting:[ 0 ]

let dfa2 =
  A.create_exn ~alphabet ~states:states2 ~transitions:transitions2 ~start:0
    ~accepting:[ 0 ]

let product = A.state_product dfa1 dfa2

let _ =
  let _ = List.map (fun (s1, s2) -> Printf.printf "(%d, %d) " s1 s2) product in
  Printf.printf "\n"

(* let n = 3000
   let a = 50
   let alphabet = List.init a (fun x -> x)
   let states = List.init n (fun x -> x)
   let transitions state letter = (state + letter) mod n
   let dfa = A.create_exn ~alphabet ~states ~transitions ~start:0 ~accepting:[ 0 ]
   let l = 1000007
   let word = List.init l (fun x -> x mod a)
   let final = A.step dfa word
   let accept = A.accept dfa word
   let () = Printf.printf "Final State = %n Accepting: %b\n" final accept
   let dfa_not = A.negate dfa
   let final = A.step dfa_not word
   let accept = A.accept dfa_not word
   let () = Printf.printf "Final State = %n Accepting: %b\n" final accept *)
