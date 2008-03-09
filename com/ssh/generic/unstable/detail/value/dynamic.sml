(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDynamic (Arg : WITH_DYNAMIC_DOM) = let
   structure Result = struct
      (* <-- SML/NJ workaround *)
      open TopLevel
      infix <-->
      (* SML/NJ workaround --> *)

      structure Dynamic = struct
         datatype t =
            PRODUCT    of (t, t) Product.t
          | SUM        of (t, t) Sum.t
          | UNIT
          | ARROW      of t UnOp.t
          | EXN        of Exn.t
          | LIST       of t List.t
          | VECTOR     of t Vector.t
          | FIXED_INT  of FixedInt.t
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
(*
          | WORD64     of Word64.t
*)
         exception Dynamic
      end

      open Dynamic

      val op <--> = Iso.<-->

      fun isoUnsupported text = (failing text, failing text)

      structure DynamicRep = LayerRep' (open Arg type 'a t = ('a, t) Iso.t)

      open DynamicRep.This

      fun toDynamic t = Iso.to (getT t)
      fun fromDynamic t d =
          SOME (Iso.from (getT t) d) handle Dynamic.Dynamic => NONE

      structure Open = LayerCases
        (fun iso bId aIb = bId <--> aIb
         val isoProduct = iso
         val isoSum     = iso

         fun op *` is =
             (PRODUCT, fn PRODUCT ? => ? | _ => raise Dynamic)
                <--> Product.iso is
         val T      = id
         fun R _    = id
         val tuple  = id
         val record = id

         fun op +` is =
             (SUM, fn SUM ? => ? | _ => raise Dynamic) <--> Sum.iso is
         val unit  = (fn () => UNIT, fn UNIT => () | _ => raise Dynamic)
         fun C0 _  = unit
         fun C1 _  = id
         val data  = id

         fun Y ? = let open Tie in tuple2 (function, function) end ?

         fun op --> is =
             (ARROW, fn ARROW ? => ? | _ => raise Dynamic) <--> Fn.iso is

         val exn = (EXN, fn EXN ? => ? | _ => raise Dynamic)
         fun regExn0 _ _ = ()
         fun regExn1 _ _ _ = ()

         fun list i =
             (LIST, fn LIST ? => ? | _ => raise Dynamic) <--> List.iso i
         fun vector i =
             (VECTOR, fn VECTOR ? => ? | _ => raise Dynamic) <--> Vector.iso i

         fun array _ = isoUnsupported "Dynamic.array unsupported"
         fun refc  _ = isoUnsupported "Dynamic.refc unsupported"

         val fixedInt = (FIXED_INT,  fn FIXED_INT  ? => ? | _ => raise Dynamic)
         val largeInt = (LARGE_INT,  fn LARGE_INT  ? => ? | _ => raise Dynamic)

         val largeWord = (LARGE_WORD, fn LARGE_WORD ? => ? | _ => raise Dynamic)
         val largeReal = (LARGE_REAL, fn LARGE_REAL ? => ? | _ => raise Dynamic)

         val bool   = (BOOL,   fn BOOL   ? => ? | _ => raise Dynamic)
         val char   = (CHAR,   fn CHAR   ? => ? | _ => raise Dynamic)
         val int    = (INT,    fn INT    ? => ? | _ => raise Dynamic)
         val real   = (REAL,   fn REAL   ? => ? | _ => raise Dynamic)
         val string = (STRING, fn STRING ? => ? | _ => raise Dynamic)
         val word   = (WORD,   fn WORD   ? => ? | _ => raise Dynamic)

         val word8  = (WORD8,  fn WORD8  ? => ? | _ => raise Dynamic)
         val word32 = (WORD32, fn WORD32 ? => ? | _ => raise Dynamic)
(*
         val word64 = (WORD64, fn WORD64 ? => ? | _ => raise Dynamic)
*)

         fun hole () = (undefined, undefined)

         open Arg DynamicRep)
   end
in
   Result :> DYNAMIC_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end
