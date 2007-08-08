(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDynamic (Arg : OPEN_CASES) : DYNAMIC_CASES = struct
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

   structure Dynamic = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = ('a, t) Iso.t))

   open Dynamic.This

   fun toDyn t = Iso.to (getT t)
   fun fromDyn t d = SOME (Iso.from (getT t) d) handle Dyn.Dyn => NONE

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Dynamic and Rep = Dynamic.Closed

      fun iso bId aIb = bId <--> aIb
      val isoProduct = iso
      val isoSum     = iso

      fun op *` is =
          (PRODUCT, fn PRODUCT ? => ? | _ => raise Dyn) <--> Product.iso is
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` is = (SUM, fn SUM ? => ? | _ => raise Dyn) <--> Sum.iso is
      val unit  = (fn () => UNIT, fn UNIT => () | _ => raise Dyn)
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      fun Y ? = let open Tie in tuple2 (function, function) end ?

      fun op --> is = (ARROW, fn ARROW ? => ? | _ => raise Dyn) <--> Fn.iso is

      val exn = (EXN, fn EXN ? => ? | _ => raise Dyn)
      fun regExn _ _ = ()

      fun list i = (LIST, fn LIST ? => ? | _ => raise Dyn) <--> List.iso i
      fun vector i = (VECTOR, fn VECTOR ? => ? | _ => raise Dyn) <--> Vector.iso i

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
