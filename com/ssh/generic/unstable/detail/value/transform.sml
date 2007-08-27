(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTransform (Arg : OPEN_CASES) : TRANSFORM_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   infix  0 &
   (* SML/NJ workaround --> *)

   type c = Word.t
   val ID = 0w0 and REC = 0w1 and CUSTOM = 0w2
   val join = Word.orb

   type e = Univ.t List.t
   type 'a t = c * ('a * e) UnOp.t

   fun lift f = Pair.map (f, id)

   val default = (ID, id)

   fun un f2f (c, f) = if ID = c then default else (c, f2f f)
   fun bin fs2f ((aC, aT), (bC, bT)) =
       case join (aC, bC) of c => if ID = c then default else (c, fs2f (aT, bT))

   fun cyclic t =
       case Univ.Emb.new ()
        of (to, from) => fn (x, e) => if List.exists (SOME x <\ op = o from) e
                                      then (x, e)
                                      else t (x, to x::e)

   structure Transform = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Transform.This

   fun makeTransform a2a t t2u =
       #1 o #2 (getT (t2u (mapT (const (CUSTOM, lift a2a)) t))) o id /> []

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Transform and Rep = Transform.Closed

      fun iso ? (a2b, b2a) = un (Fn.map (lift a2b, lift b2a)) ?
      val isoProduct = iso
      val isoSum     = iso

      fun op *` ? =
          bin (fn (aT, bT) => fn (a & b, e) => let
                                    val (a, e) = aT (a, e)
                                    val (b, e) = bT (b, e)
                                 in
                                    (a & b, e)
                                 end) ?
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` ? =
          bin (fn (aT, bT) => fn (INL a, e) => lift INL (aT (a, e))
                               | (INR b, e) => lift INR (bT (b, e))) ?
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      fun Y ? = Tie.pure (fn () => let
                                val r = ref (raising Fix.Fix)
                             in
                                ((REC, fn x => !r x),
                                 fn (c, f) =>
                                    if c <= REC
                                    then default
                                    else (r := f ; (CUSTOM, f)))
                             end) ?

      fun op --> _ = (ID, failing "Transform.--> has no default")

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list ? =
          un (fn xT =>
                 Pair.map (id, Pair.snd) o
                 List.unfoldr'
                    (fn ([],    _) => NONE
                      | (x::xs, e) =>
                        case xT (x, e) of (y, e) => SOME (y, (xs, e)))) ?

      fun vector ? =
          un (fn xT => fn (v, e) =>
                 Vector.unfoldi
                    (xT o lift (v <\ Vector.sub)) (Vector.length v, e)) ?

      fun array ? =
          un (fn aT =>
                 cyclic (fn (aA, e) => let
                               fun lp (i, e) =
                                   if i = Array.length aA then e else
                                   case aT (Array.sub (aA, i), e)
                                    of (a, e) => (Array.update (aA, i, a)
                                                ; lp (i+1, e))
                            in
                               (aA, lp (0, e))
                            end)) ?

      fun refc ? =
          un (fn aT =>
                 cyclic
                    (fn (r, e) =>
                        case aT (!r, e) of (a, e) => (r := a ; (r, e)))) ?

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
