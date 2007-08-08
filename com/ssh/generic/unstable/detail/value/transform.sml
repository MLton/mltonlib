(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTransform (Arg : OPEN_CASES) : TRANSFORM_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Transform = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (UnOp))

   fun makeTransform a2a tA tA2tB = let
      val tA = Transform.This.mapT (const a2a) tA
      val tB = tA2tB tA
   in
      Transform.This.getT tB
   end

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Transform and Rep = Transform.Closed

      fun iso rB aIb = Fn.map aIb rB
      val isoProduct = iso
      val isoSum     = iso

      val op *` = Product.map
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      val op +` = Sum.map
      val unit  = id
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Transform.--> not yet implemented"

      fun regExn _ _ = ()
      fun exn _ = fail "Transform.exn not yet implemented"

      val list   =   List.map
      val vector = Vector.map

      fun array tA x = (Array.modify tA x ; x)
      fun refc  tA x =   (Ref.modify tA x ; x)

      val largeInt  = id
      val largeReal = id
      val largeWord = id

      val bool   = id
      val char   = id
      val int    = id
      val real   = id
      val string = id
      val word   = id

      val word8  = id
      val word32 = id
      val word64 = id)

   open Layered
end
