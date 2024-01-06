(** Generic module signature for elements of a total order *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end


(** Signature of the module obtained from Fsa.Make *)
module type F = sig
    type alphabet
    type state
  
    type t
    (** type of FSA *)
  
    val create :
      alphabet:alphabet list ->
      states:state list ->
      start:state ->
      accepting:state list ->
      transitions:(state -> alphabet -> state) ->
      (t, string) Result.t
  end