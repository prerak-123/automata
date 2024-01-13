(** Generic module signature for elements of a total order *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(** Signature of module for DFA *)
module type DFA = sig
  exception Invalid_letter
  exception Invalid_state
  exception Invalid_DFA of string

  type letter
  type state
  type t

  val create_exn :
    alphabet:letter list ->
    states:state list ->
    start:state ->
    accepting:state list ->
    transitions:(state -> letter -> state) ->
    t

  val letter_compare : letter -> letter -> int
  val state_compare : state -> state -> int
  val alphabet_list : t -> letter list
  val state_list : t -> state list
  val start : t -> state
  val accepting_list : t -> state list
  val transitions : t -> state -> letter -> state
  val accepts : t -> letter list -> bool
  val reachable : t -> state list
  val step : t -> ?start:state -> letter list -> state
  val negate : t -> t
  val minimize : t -> t
end

(** Signature of module for NFA *)
module type NFA = sig
  exception Invalid_NFA of string
  exception Invalid_letter
  exception Invalid_states

  type letter
  type state
  type t

  val create_exn :
    alphabet:letter list ->
    states:state list ->
    start:state list ->
    accepting:state list ->
    transitions:(state -> letter -> state list) ->
    t

  val alphabet_list : t -> letter list
  val state_list : t -> state list
  val start_list : t -> state list
  val accepting_list : t -> state list
  val transitions : t -> state -> letter -> state list
  val step : t -> ?start:state list -> letter list -> state list
  val accepts : t -> letter list -> bool
  (* val letter_compare : letter -> letter -> int
  val state_compare : state -> state -> int
  val reachable : t -> state list
  val concatenate : t -> t -> t
  val kleene_closure : t -> state * state -> t *)
end
