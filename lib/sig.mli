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

  val step : t -> ?start:state -> letter list -> state
  val accept : t -> letter list -> bool
  val state_product : t -> t -> (state * state) list
  val negate : t -> t
end
