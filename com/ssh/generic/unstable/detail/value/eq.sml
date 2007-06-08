(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Eq :> EQ_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open Basic Fn Product Sum UnPr
   infix  7 *`
   infix  6 +`
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure Lifted = LiftGeneric
     (structure Index = struct
         type 'a t = 'a BinPr.t
         type 'a s = 'a t
         type ('a, 'k) p = 'a t
      end

      fun iso b (a2b, _) = b o Pair.map (Sq.mk a2b)

      val op *` = Product.equal
      val op +` = Sum.equal

      val Y = Tie.function

      local
         val e = Fail "Eq.--> not supported"
      in
         fun _ --> _ = raising e
      end

      val exn : Exn.t Index.t Ref.t = ref GenericsUtil.failExnSq
      fun regExn t (_, prj) =
          Ref.modify (fn exn =>
                         fn (l, r) =>
                            case prj l & prj r of
                               SOME l & SOME r => t (l, r)
                             | SOME _ & NONE   => false
                             | NONE   & SOME _ => false
                             | NONE   & NONE   => exn (l, r)) exn
      val exn = fn ? => !exn ?

      fun array _ = op =
      fun refc _ = op =

      val list = ListPair.allEq

      fun vector eq = iso (list eq) Vector.isoList (* XXX can be optimized *)

      val bool   = op =
      val char   = op =
      val int    = op =
      val real   = Real.==
      val string = op =
      val unit   = op =
      val word   = op =

      val largeInt  = op =
      val largeReal = LargeReal.==
      val largeWord = op =

      val word8  = op =
   (* val word16 = op = (* Word16 not provided by SML/NJ *) *)
      val word32 = op =
      val word64 = op =

      (* Trivialities *)

      val isoProduct = iso
      val isoSum = iso

      val T = id
      fun R _ = id
      val tuple = id
      val record = id

      fun C0 _ = unit
      fun C1 _ = id
      val data = id)

   open Lifted

   structure Eq = Index

   val eq = Pair.fst
   fun notEq (eq, _) = negate eq
end

functor WithEq (Outer : EXT_GENERIC) :> EQ_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Outer and Inner = Eq)
   open Eq Joined
   structure Eq = Index
   fun mk f = f o Outer.Index.getT
   val eq    = fn ? => mk eq    ?
   val notEq = fn ? => mk notEq ?
end
