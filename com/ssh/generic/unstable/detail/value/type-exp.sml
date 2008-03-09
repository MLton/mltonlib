(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeExp (Arg : WITH_TYPE_EXP_DOM) = let
   structure Result = struct
      (* <-- SML/NJ workaround *)
      open TopLevel
      (* SML/NJ workaround --> *)

      open Generics Ty open Product Sum Con0 Con1 Con2

      structure TypeVar = struct
         type t = Unit.t Ref.t
         val new = ref
      end

      fun mapElem f =
       fn TIMES (a, b)  => TIMES (mapElem f a, mapElem f b)
        | ISO_PRODUCT b => ISO_PRODUCT (mapElem f b)
        | ELEM e        => ELEM (f e)

      structure TypeExpRep = LayerRep
        (open Arg
         type  'a      t = TypeVar.t Ty.t
         type  'a      s = TypeVar.t Ty.t Sum.t
         type ('a, 'k) p = (Label.t Option.t * TypeVar.t Ty.t) Product.t)

      val ty = TypeExpRep.This.getT

      structure Open = LayerCases
        (fun iso        bT _ = ISO         bT
         fun isoProduct bP _ = ISO_PRODUCT bP
         fun isoSum     bS _ = ISO_SUM     bS

         val op *` = TIMES
         fun T aT = ELEM (NONE, aT)
         fun R l aT = ELEM (SOME l, aT)
         fun tuple aP = TUPLE (mapElem Pair.snd aP)
         fun record aP = RECORD (mapElem (Pair.map (valOf, id)) aP)

         val op +` = PLUS
         val unit = CON0 UNIT
         fun C0 c = Sum.C0 c
         fun C1 c aT = Sum.C1 (c, aT)
         val data = DATA

         val Y = Tie.pure (fn () => let
                                 val v = TypeVar.new ()
                              in
                                 (VAR v, fn e => FIX (v, e))
                              end)

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
(*
         val word64 = CON0 WORD64
*)

         fun hole () = CON0 UNIT

         open Arg TypeExpRep)
   end
in
   Result :> TYPE_EXP_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end
