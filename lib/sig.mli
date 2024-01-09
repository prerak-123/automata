(** Generic module signature for elements of a total order *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(** Signature of the module obtained from Fsa.Make *)
module type F = sig
  type letter
  type state

  type t
  (** type of FSA *)

  exception Invalid_DFA of string
  exception Invalid_letter
  exception Invalid_state

  val create_exn :
    alphabet:letter list ->
    states:state list ->
    start:state ->
    accepting:state list ->
    transitions:(state -> letter -> state) ->
    t

  val alphabet_list : t -> letter list
  val state_list : t -> state list
  val start : t -> state
  val accepting_list : t -> state list
  val transitions : t -> state -> letter -> state
  val step : t -> ?start:state -> letter list -> state
  val accepts : t -> letter list -> bool
  val negate : t -> t
  val reachable : t -> state list
  val letter_compare : letter -> letter -> int
  val state_compare : state -> state -> int
end
