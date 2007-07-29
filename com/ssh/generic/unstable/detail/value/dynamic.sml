(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDynamic (Arg : OPEN_GENERIC) : DYNAMIC_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix <-->
   (* SML/NJ workaround --> *)

   structure Dyn = struct
      datatype t =
         PRODUCT    of (t, t) Product.t
       | SUM        of (t, t) Sum.t
       | UNIT
       | ARROW      of t UnOp.t
       | EXN        of Exn.t
       | LIST       of t List.t
       | VECTOR     of t Vector.t
       | LARGE_INT  of LargeInt.t
       | LARGE_WORD of LargeWord.t
       | LARGE_REAL of LargeReal.t
       | BOOL       of Bool.t
       | CHAR       of Char.t
       | INT        of Int.t
       | REAL       of Real.t
       | STRING     of String.t
       | WORD       of Word.t
       | WORD8      of Word8.t
       | WORD32     of Word32.t
       | WORD64     of Word64.t
      exception Dyn
   end

   open Dyn

   val op <--> = Iso.<-->

   fun isoUnsupported text = (failing text, failing text)

   structure Dynamic =
      LayerGenericRep
         (structure Outer = Arg.Rep
          structure Closed = MkClosedGenericRep (type 'a t = ('a, t) Iso.t))

   open Dynamic.This

   fun toDyn t = Iso.to (getT t)
   fun fromDyn t d = SOME (Iso.from (getT t) d) handle Dyn.Dyn => NONE

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Dynamic and Rep = Dynamic.Closed

      fun iso bId aIb = Iso.<--> (bId, aIb)
      val isoProduct = iso
      val isoSum     = iso

      fun op *` ((l2d, d2l), (r2d, d2r)) =
          (PRODUCT, fn PRODUCT ? => ? | _ => raise Dyn) <-->
          (Product.map (l2d, r2d), Product.map (d2l, d2r))
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` ((l2d, d2l), (r2d, d2r)) =
          (SUM, fn SUM ? => ? | _ => raise Dyn) <-->
          (Sum.map (l2d, r2d), Sum.map (d2l, d2r))
      val unit  = (fn () => UNIT, fn UNIT => () | _ => raise Dyn)
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      fun Y ? = let open Tie in tuple2 (function, function) end ?

      fun op --> ((a2d, d2a), (b2d, d2b)) =
          (ARROW, fn ARROW ? => ? | _ => raise Dyn) <-->
          (Fn.map (d2a, b2d), Fn.map (a2d, d2b))

      val exn = (EXN, fn EXN ? => ? | _ => raise Dyn)
      fun regExn _ _ = ()

      fun list (x2d, d2x) =
          (LIST, fn LIST ? => ? | _ => raise Dyn) <-->
          (List.map x2d, List.map d2x)
      fun vector (x2d, d2x) =
          (VECTOR, fn VECTOR ? => ? | _ => raise Dyn) <-->
          (Vector.map x2d, Vector.map d2x)

      fun array _ = isoUnsupported "Dyn.array unsupported"
      fun refc  _ = isoUnsupported "Dyn.refc unsupported"

      val largeInt  = (LARGE_INT,  fn LARGE_INT  ? => ? | _ => raise Dyn)
      val largeWord = (LARGE_WORD, fn LARGE_WORD ? => ? | _ => raise Dyn)
      val largeReal = (LARGE_REAL, fn LARGE_REAL ? => ? | _ => raise Dyn)

      val bool   = (BOOL,   fn BOOL   ? => ? | _ => raise Dyn)
      val char   = (CHAR,   fn CHAR   ? => ? | _ => raise Dyn)
      val int    = (INT,    fn INT    ? => ? | _ => raise Dyn)
      val real   = (REAL,   fn REAL   ? => ? | _ => raise Dyn)
      val string = (STRING, fn STRING ? => ? | _ => raise Dyn)
      val word   = (WORD,   fn WORD   ? => ? | _ => raise Dyn)

      val word8  = (WORD8,  fn WORD8  ? => ? | _ => raise Dyn)
      val word32 = (WORD32, fn WORD32 ? => ? | _ => raise Dyn)
      val word64 = (WORD64, fn WORD64 ? => ? | _ => raise Dyn))

   open Layered
end
