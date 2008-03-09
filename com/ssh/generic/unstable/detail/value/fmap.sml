(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure FmapAux = struct
   datatype u =
      PRODUCT    of (u, u) Product.t
    | SUM        of (u, u) Sum.t
    | UNIT
    | ARROW      of u UnOp.t
    | EXN        of Exn.t
    | LIST       of u List.t
    | VECTOR     of u Vector.t
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
    | ARGUMENT   of Univ.t
   datatype 'a i = ISO of ('a, u) Iso.t
   datatype 'a t = IN of 'a
end

signature FMAP_CASES = FMAP_CASES
   where type 'a Fmap.i = 'a FmapAux.i
   where type 'a Fmap.t = 'a FmapAux.t

signature MK_FMAP_DOM = MK_FMAP_DOM
   where type 'a Fmap.i = 'a FmapAux.i
   where type 'a Fmap.t = 'a FmapAux.t

functor WithFmap (Arg : WITH_FMAP_DOM) = let
   structure Result = struct
      (* <-- SML/NJ workaround *)
      open TopLevel
      infix <-->
      (* SML/NJ workaround --> *)

      val op <--> = Iso.<-->

      structure FmapRep = LayerRep' (open Arg type 'a t = 'a FmapAux.i)

      structure Fmap = struct
         open FmapAux
         val get = IN FmapRep.This.getT
         val map = IN FmapRep.This.mapT
      end

      open Fmap

      fun isoUnsupported text = ISO (failing text, failing text)

      structure Open = LayerCases
        (fun iso (ISO bId) aIb = ISO (bId <--> aIb)
         val isoProduct = iso
         val isoSum     = iso

         fun op *` (ISO a, ISO b) =
             ISO ((PRODUCT, fn PRODUCT ? => ? | _ => raise Empty)
                     <--> Product.iso (a, b))
         val T      = id
         fun R _    = id
         val tuple  = id
         val record = id

         fun op +` (ISO a, ISO b) =
             ISO ((SUM, fn SUM ? => ? | _ => raise Empty) <--> Sum.iso (a, b))
         val unit  = ISO (fn () => UNIT, fn UNIT => () | _ => raise Empty)
         fun C0 _  = unit
         fun C1 _  = id
         val data  = id

         fun Y ? = let open Tie in iso (tuple2 (function, function)) end
                      (fn ISO ? => ?, ISO) ?

         fun op --> (ISO a, ISO b) =
             ISO ((ARROW, fn ARROW ? => ? | _ => raise Empty) <--> Fn.iso (a, b))

         val exn = ISO (EXN, fn EXN ? => ? | _ => raise Empty)
         fun regExn0 _ _ = ()
         fun regExn1 _ _ _ = ()

         fun list (ISO i) =
             ISO ((LIST, fn LIST ? => ? | _ => raise Empty) <--> List.iso i)
         fun vector (ISO i) =
             ISO ((VECTOR, fn VECTOR ? => ? | _ => raise Empty) <--> Vector.iso i)

         fun array _ = isoUnsupported "Fmap.array unsupported"
         fun refc  _ = isoUnsupported "Fmap.refc unsupported"

         val fixedInt = ISO (FIXED_INT,  fn FIXED_INT  ? => ? | _ => raise Empty)
         val largeInt = ISO (LARGE_INT,  fn LARGE_INT  ? => ? | _ => raise Empty)

         val largeWord = ISO (LARGE_WORD, fn LARGE_WORD ? => ? | _ => raise Empty)
         val largeReal = ISO (LARGE_REAL, fn LARGE_REAL ? => ? | _ => raise Empty)

         val bool   = ISO (BOOL,   fn BOOL   ? => ? | _ => raise Empty)
         val char   = ISO (CHAR,   fn CHAR   ? => ? | _ => raise Empty)
         val int    = ISO (INT,    fn INT    ? => ? | _ => raise Empty)
         val real   = ISO (REAL,   fn REAL   ? => ? | _ => raise Empty)
         val string = ISO (STRING, fn STRING ? => ? | _ => raise Empty)
         val word   = ISO (WORD,   fn WORD   ? => ? | _ => raise Empty)

         val word8  = ISO (WORD8,  fn WORD8  ? => ? | _ => raise Empty)
         val word32 = ISO (WORD32, fn WORD32 ? => ? | _ => raise Empty)
(*
         val word64 = ISO (WORD64, fn WORD64 ? => ? | _ => raise Empty)
*)

         fun hole () = ISO (undefined, undefined)

         open Arg FmapRep)
   end
in
   Result :> FMAP_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end

functor MkFmap (Arg : MK_FMAP_DOM) : sig
   val map : ('a -> 'b) -> 'a Arg.t -> 'b Arg.t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   open FmapAux Arg

   fun map a2b = let
      val (fromB, toB) = Univ.Iso.new ()
      val IN get = Fmap.get and IN map = Fmap.map
      fun mk i = get (t (map (const (ISO i)) (Open.hole ())))
      val ISO (fromA, _) = mk (ARGUMENT o fromB o a2b, undefined)
      val ISO (_, toB) = mk (undefined, fn ARGUMENT ? => toB ? | _ => raise Empty)
   in
      toB o fromA
   end
end

structure FmapAux : sig type 'a i and 'a t end = FmapAux
