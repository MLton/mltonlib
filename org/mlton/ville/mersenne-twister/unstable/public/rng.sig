(*
 * A minimal signature for an imperative random number generator
 * objects.
 *)
signature RNG = sig
  type t

  val new: word -> t
  (**
   * Creates a new random number generator and seeds it with the
   * given seed.
   *)

  val rand: t -> word
  (** Returns the next 32 bit random number. *)

end
