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

   (** == Conversions == *)

   val toList : 'a vector -> 'a list
   (**
    * Generates a list from the given vector.  Specifically, the result of
    * {toList v} is equivalent to {foldr op :: [] v}.
    *)

   (** == Isomorphisms == *)

   val isoList : ('a vector, 'a list) iso
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)
end
