(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Imperative resizable array.
 *)
signature RESIZABLE_ARRAY = sig
   include BUFFER

   (** == Mutators == *)

   val update : ('a t * Int.t * 'a) Effect.t
   (**
    * {update (a, i, v)} Sets the {i}th element of the resizable array {a}
    * to {v}.  If {i < 0} or {length a <= i}, then the {Subscript}
    * exception is raised.
    *)

   val pop : 'a t -> 'a Option.t
   (**
    * Removes the last element {v} of the resizable array and returns
    * {SOME v} or {NONE} if the resizable array is empty.
    *)
end
