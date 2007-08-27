(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTransform (Arg : OPEN_CASES) : TRANSFORM_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 4 <\
   (* SML/NJ workaround --> *)

   type e = Univ.t List.t
   type 'a t = ('a * e) UnOp.t

   fun lift f = Pair.map (f, id)

   val default = id

   fun cyclic t = let
      val (to, from) = Univ.Emb.new ()
   in
      fn (x, e) => if List.exists (SOME x <\ op = o from) e
                   then (x, e)
                   else t (x, to x::e)
   end

   structure Transform = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   fun makeTransform a2a tA tA2tB = let
      val tA = Transform.This.mapT (const (lift a2a)) tA
      val tB = tA2tB tA
   in
      Pair.fst o Transform.This.getT tB o (fn b => (b, []))
   end

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Transform and Rep = Transform.Closed

      fun iso bT (a2b, b2a) = lift b2a o bT o lift a2b
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (aT, bT) (a & b, e) = let
         val (a, e) = aT (a, e)
         val (b, e) = bT (b, e)
      in
         (a & b, e)
      end
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (aT, bT) (s, e) =
          case s
           of INL a => lift INL (aT (a, e))
            | INR b => lift INR (bT (b, e))
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Transform.--> has no default"

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list xT = Pair.map (id, Pair.snd) o
                    List.unfoldr'
                       (fn ([],    _) => NONE
                         | (x::xs, e) =>
                           case xT (x, e) of (y, e) => SOME (y, (xs, e)))

      fun vector xT (v, e) =
          Vector.unfoldi (xT o lift (v <\ Vector.sub)) (Vector.length v, e)

      fun array aT = cyclic (fn (aA, e) => let
                                   fun lp (i, e) =
                                       if i = Array.length aA then e else
                                       case aT (Array.sub (aA, i), e)
                                        of (a, e) => (Array.update (aA, i, a)
                                                    ; lp (i+1, e))
                                in
                                   (aA, lp (0, e))
                                end)

      fun refc aT =
          cyclic (fn (r, e) => case aT (!r, e) of (a, e) => (r := a ; (r, e)))

      val fixedInt = default
      val largeInt = default

      val largeReal = default
      val largeWord = default

      val bool   = default
      val char   = default
      val int    = default
      val real   = default
      val string = default
      val word   = default

      val word8  = default
      val word32 = default
      val word64 = default)

   open Layered
end
