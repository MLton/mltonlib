(* Combinators for converting primitive random number generators to
 * more useful ones. *)
signature RANDOM = sig
   structure RNG : RNG
   type 'a t

   val generate : RNG.t -> 'a t -> 'a
   (** Generates the next random value. *)

   val intInRange : int * int -> int t
   (** {intInRange (a, b)} generates integers in the range [a, b). *)

   val realInRange : real * real -> real t
   (** {realInRange (a, b)} generates reals in the range [a, b). *)

   val wordInRange : word * word -> word t
   (** {wordInRange (a, b)} generates words in the range [a, b). *)

   val standardNormal : real t
   (** Generates reals from the standard normal distribution. *)

   val idString : int -> string t
   (**
    * {idString n} generates random strings of length {n} with {n * 6}
    * bits of entropy.  This is intended for generating unique
    * printable identifiers.
    *)

end
