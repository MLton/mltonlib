(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeExp (Arg : OPEN_CASES) : TYPE_EXP_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   open Ty Ty.Con0 Ty.Con1 Ty.Con2

   fun mapElem f =
    fn Product.*` (a, b) => Product.*` (mapElem f a, mapElem f b)
     | Product.ISO b     => Product.ISO (mapElem f b)
     | Product.ELEM e    => Product.ELEM (f e)

   structure TypeExp = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = struct
         type 'a t = Var.t Ty.t
         type 'a s = Var.t Ty.t Ty.Sum.t
         type ('a, 'k) p = (Generics.Label.t Option.t * Var.t Ty.t) Ty.Product.t
      end)

   val ty = TypeExp.This.getT

   structure Layered = LayerCases
     (structure Outer = Arg and Result = TypeExp and Rep = TypeExp.Closed

      fun iso        bT _ =         ISO bT
      fun isoProduct bP _ = Product.ISO bP
      fun isoSum     bS _ =     Sum.ISO bS

      fun op *` (aT, bT) = Product.*` (aT, bT)
      fun T aT   = Product.ELEM (NONE, aT)
      fun R l aT = Product.ELEM (SOME l, aT)
      fun tuple aP = TUPLE (mapElem Pair.snd aP)
      fun record aP = RECORD (mapElem (Pair.map (valOf, id)) aP)

      fun op +` (aT, bT) = Sum.+` (aT, bT)
      val unit  = CON0 UNIT
      fun C0 c  = Sum.C0 c
      fun C1 c aT = Sum.C1 (c, aT)
      val data = DATA

      fun Y ? =
          Tie.pure (fn () => let
                          val v = Var.new ()
                       in
                          (VAR v, fn e => FIX (v, e))
                       end) ?

      fun op --> aTbT = CON2 (ARROW, aTbT)

      val exn = CON0 EXN
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list aT = CON1 (LIST, aT)
      fun vector aT = CON1 (VECTOR, aT)
      fun array aT = CON1 (ARRAY, aT)
      fun refc  aT = CON1 (REF, aT)

      val fixedInt = CON0 FIXED_INT
      val largeInt = CON0 LARGE_INT

      val largeReal = CON0 LARGE_REAL
      val largeWord = CON0 LARGE_WORD

      val bool   = CON0 BOOL
      val char   = CON0 CHAR
      val int    = CON0 INT
      val real   = CON0 REAL
      val string = CON0 STRING
      val word   = CON0 WORD

      val word8  = CON0 WORD8
      val word32 = CON0 WORD32
      val word64 = CON0 WORD64)

   open Layered
end
