(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Utility module for defining "variadic" type-indexed functions in SML.
 *
 * See
 *
 *   http://mlton.org/Fold
 *
 * for extensive discussion of the subject.
 *)

structure Fold = struct
   type ('a, 'b, 'c, 'd) step =
        'a * ('b -> 'c) -> 'd
   type ('a, 'b, 'c, 'd) t =
        ('a, 'b, 'c, 'd) step -> 'd
   type ('a, 'b, 'c, 'd, 'e) step0 =
        ('a, 'c, 'd, ('b, 'c, 'd, 'e) t) step
   type ('a, 'b, 'c, 'd, 'e, 'f) step1 =
        ('b, 'd, 'e, 'a -> ('c, 'd, 'e, 'f) t) step
end

signature FOLD = sig
   type ('a, 'b, 'c, 'd) step =
        ('a, 'b, 'c, 'd) Fold.step
   type ('a, 'b, 'c, 'd) t =
        ('a, 'b, 'c, 'd) Fold.t
   type ('a, 'b, 'c, 'd, 'e) step0 =
        ('a, 'b, 'c, 'd, 'e) Fold.step0
   type ('a, 'b, 'c, 'd, 'e, 'f) step1 =
        ('a, 'b, 'c, 'd, 'e, 'f) Fold.step1

   val fold : 'a * ('b -> 'c) -> ('a, 'b, 'c, 'd) t
   val unfold : ('a, 'b, 'c, 'a * ('b -> 'c)) t
                -> 'a * ('b -> 'c)
   val lift : ('a, 'b, 'c, 'a * ('b -> 'c)) t
              -> ('a, 'b, 'c, 'd) t

   val post : ('a -> 'd)
              -> ('b, 'c, 'a, 'b * ('c -> 'a)) t
              -> ('b, 'c, 'd, 'e) t

   val step0 : ('a -> 'b)
               -> ('a, 'b, 'c, 'd, 'e) step0
   val step1 : ('a * 'b -> 'c)
               -> ('a, 'b, 'c, 'd, 'e, 'f) step1

   val unstep0 : ('a, 'b, 'b, 'b, 'b) step0
                 -> 'a -> 'b
   val unstep1 : ('a, 'b, 'c, 'c, 'c, 'c) step1
                 -> 'a * 'b -> 'c

   val lift0 : ('a, 'b, 'b, 'b, 'b) step0
               -> ('a, 'b, 'c, 'd, 'e) step0
   val lift1 : ('a, 'b, 'c, 'c, 'c, 'c) step1
               -> ('a, 'b, 'c, 'd, 'e, 'f) step1
   val lift0to1 : ('b, 'c, 'c, 'c, 'c) step0
                  -> ('a, 'b, 'c, 'd, 'e, 'f) step1
end

fun $ (x, f) = f x

structure Fold :> FOLD = struct
   open Fold

   val fold = pass
   fun unfold f = f id
   fun lift ? = (fold o unfold) ?

   fun post g = fold o Pair.map (id, fn f => g o f) o unfold

   fun step0 h (a1, f) = fold (h a1, f)
   fun step1 h (a2, f) a1 = fold (h (a1, a2), f)

   fun unstep0 s a1 = fold (a1, id) s $
   fun unstep1 s (a1, a2) = fold (a2, id) s a1 $

   fun lift0 ? = (step0 o unstep0) ?
   fun lift1 ? = (step1 o unstep1) ?

   fun lift0to1 s = step1 (unstep0 s o Pair.snd)
end
