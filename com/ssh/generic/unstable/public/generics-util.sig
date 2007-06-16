(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for utilities for defining generic values.
 *)
signature GENERICS_UTIL = sig
   (** == For Defining Closed Generic Functions == *)

   val failExn : Exn.t -> 'a
   val failExnSq : Exn.t Sq.t -> 'a

   (** == For Defining Open Generic Functions == *)

   val op0 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   val op1 : ((('a, 'b) Pair.t -> ('c, 'd) Pair.t) -> 'e)
             -> ('a -> 'c) -> ('b -> 'd) -> 'e
   val op2 : ((('a, 'b) Pair.t * ('c, 'd) Pair.t -> ('e, 'f) Pair.t) -> 'g)
             -> ('a * 'c -> 'e) -> ('b * 'd -> 'f) -> 'g

   val t : ((('a, 'b) Pair.t -> ('c, 'd) Pair.t) -> 'e)
           -> ('a -> 'c) -> ('b -> 'd) -> 'e
   val r : (('a -> ('b, 'c) Pair.t -> ('d, 'e) Pair.t) -> 'f)
           -> ('a -> 'b -> 'd) -> ('a -> 'c -> 'e) -> 'f

   val c0 : (('a -> ('b, 'c) Pair.t) -> 'd) -> ('a -> 'b) -> ('a -> 'c) -> 'd
   val c1 : (('a -> ('b, 'c) Pair.t -> ('d, 'e) Pair.t) -> 'f)
            -> ('a -> 'b -> 'd) -> ('a -> 'c -> 'e) -> 'f

   val y : (('a * 'b) Tie.t -> 'c) -> 'a Tie.t -> 'b Tie.t -> 'c

   val morph : (('a * 'b -> 'c -> 'd * 'e) -> 'f)
               -> ('a -> 'c -> 'd) -> ('b -> 'c -> 'e) -> 'f

   val re : (('a * 'b -> 'c -> Unit.t) -> 'd)
            -> ('a -> 'c -> Unit.t) -> ('b -> 'c -> Unit.t) -> 'd
end
