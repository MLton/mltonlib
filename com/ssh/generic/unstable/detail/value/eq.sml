(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithEq (Arg : WITH_EQ_DOM) : EQ_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   fun sequ (Ops.S {length, sub, ...}) eq (l, r) = let
      val lL = length l
      val lR = length r
      fun lp i = let
         val i = i-1
      in
         i < 0 orelse eq (sub (l, i), sub (r, i))
                      andalso lp i
      end
   in
      lL = lR andalso lp lL
   end

   fun iso b (a2b, _) = BinPr.map a2b b

   val mkReal =
    fn Ops.R {isoBits = SOME isoBits, ...} => iso op = isoBits
     | Ops.R {toBytes, ...} => iso op = (toBytes, undefined)

   val exnHandler : Exn.t BinPr.t Ref.t = ref GenericsUtil.failExnSq
   fun regExn t (_, e2to) =
       Ref.modify (fn exnHandler =>
                   fn (l, r) =>
                      case e2to l & e2to r
                       of NONE   & NONE   => exnHandler (l, r)
                        | SOME l & SOME r => t (l, r)
                        | _               => false) exnHandler

   structure EqRep = LayerRep' (open Arg BinPr)

   open EqRep.This

   val eq = getT
   fun notEq t = not o eq t
   fun withEq eq = mapT (const eq)

   structure Open = LayerCases
     (val iso        = iso
      val isoProduct = iso
      val isoSum     = iso

      val op *`  = Product.equal
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      val op +` = Sum.equal
      val unit  = op = : Unit.t t
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Eq.--> unsupported"

      fun exn ? = !exnHandler ?
      fun regExn0 _ = regExn unit
      fun regExn1 _ = regExn

      val list = ListPair.allEq

      fun vector ? = sequ VectorOps.ops ?

      fun array _ = op = : 'a Array.t t
      fun refc  _ = op = : 'a Ref.t t

      val fixedInt = op = : FixedInt.t t
      val largeInt = op = : LargeInt.t t

      val largeReal = mkReal LargeRealOps.ops
      val largeWord = op = : LargeWord.t t

      val bool   = op = : Bool.t t
      val char   = op = : Char.t t
      val int    = op = : Int.t t
      val real   = mkReal RealOps.ops
      val string = op = : String.t t
      val word   = op = : Word.t t

      val word8  = op = : Word8.t t
      val word32 = op = : Word32.t t
(*
      val word64 = op = : Word64.t t
*)

      fun hole () = undefined

      open Arg EqRep)
end
