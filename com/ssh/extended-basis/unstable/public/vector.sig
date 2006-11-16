(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {VECTOR} signature.
 *)
signature VECTOR = sig
   include VECTOR

   type 'a t = 'a vector
   (**
    * Convenience alias.
    *)

   val unfoldi : (int * 'b -> 'a * 'b) -> int * 'b -> 'a t * 'b
   (**
    * {unfoldi f (n, b)} constructs a vector {v} of a length {n}, whose
    * elements {vi} are determined by the equations {b0 = b} and {(vi,
    * bi+1) = f (i, bi)}.
    *)

   (** == Conversions == *)

   val toList : 'a vector -> 'a list
   (**
    * Generates a list from the given vector.  Specifically, the result of
    * {toList v} is equivalent to {foldr op :: [] v}.
    *)

   (** == Isomorphisms == *)

   val isoList : ('a vector, 'a list) Iso.t
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)
end
