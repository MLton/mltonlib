(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Pair : PAIR = struct
   open Pair

   val isoTuple2 as (fromTuple2, toTuple2) =
       (fn (a, b) => (a, b),
        fn (a, b) => (a, b))
   val isoTuple3 as (fromTuple3, toTuple3) =
       (fn (a, b, c) => ((a, b), c),
        fn ((a, b), c) => (a, b, c))
   val isoTuple4 as (fromTuple4, toTuple4) =
       (fn (a, b, c, d) => (((a, b), c), d),
        fn (((a, b), c), d) => (a, b, c, d))

   fun swap (a, b) = (b, a)
   fun swizzle ((a, b), (c, d)) = ((a, c), (b, d))

   fun fst (a, _) = a
   fun snd (_, b) = b

   fun app (ea, eb) (a, b) = (ea a : unit ; eb b : unit)
   fun appFst eA = app (eA, Effect.ignore)
   fun appSnd eB = app (Effect.ignore, eB)

   fun map (fa, fb) (a, b) = (fa a, fb b)
   fun mapFst fA = map (fA, Fn.id)
   fun mapSnd fB = map (Fn.id, fB)

   local
      fun mk p (fA, fB) (a, b) = let
         val a = fA a
      in
         if p a then a else fB b
      end
   in
      fun all     ? = mk Bool.isFalse  ?
      fun exists  ? = mk Bool.isTrue   ?
      fun equal   ? = mk Bool.isFalse  ? o swizzle
      fun collate ? = mk Order.isEqual ? o swizzle
   end

   fun foldl (fa, fb) ((a, b), s) = fb (b, fa (a, s))
   fun foldr (fa, fb) ((a, b), s) = fa (a, fb (b, s))

   fun thunk (na, nb) () = (na (), nb ())
end
