(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ord :> ORD_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open Basic Fn Product Sum UnPr
   infix  7 *`
   infix  6 +`
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure Lifted = LiftGeneric
     (structure Index = struct
         type 'a t = 'a Cmp.t
         type 'a s = 'a t
         type ('a, 'k) p = 'a t
      end

      fun inj b a2b = b o Pair.map (Sq.mk a2b)
      fun iso b = inj b o Iso.to

      val op *` = Product.collate
      val op +` = Sum.collate

      val Y = Tie.function

      local
         val e = Fail "Compare.--> not supported"
      in
         fun _ --> _ = raising e
      end

     (* XXX It is also possible to implement exn so that compare provides
      * a reasonable answer as long as at least one of the exception
      * variants (involved in a comparison) has been registered.
      *)
      val exn : Exn.t Index.t Ref.t = ref GenericsUtil.failExnSq
      fun regExn t (_, prj) =
          Ref.modify (fn exn =>
                         fn (l, r) =>
                            case prj l & prj r of
                               SOME l & SOME r => t (l, r)
                             | SOME _ & NONE   => GREATER
                             | NONE   & SOME _ => LESS
                             | NONE   & NONE   => exn (l, r)) exn
      val exn = fn ? => !exn ?

      val array  = Array.collate
      fun refc ? = inj ? !

      val vector = Vector.collate

      val list = List.collate

      val unit   = fn ((), ()) => EQUAL
      val bool   = Bool.compare
      val char   = Char.compare
      val int    = Int.compare
      val real   = Real.compare
      val string = String.compare
      val word   = Word.compare

      val largeInt  = LargeInt.compare
      val largeReal = LargeReal.compare
      val largeWord = LargeWord.compare

      val word8  = Word8.compare
   (* val word16 = Word16.compare *)
      val word32 = Word32.compare
      val word64 = Word64.compare

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

   structure Ord = Index

   val compare = Pair.fst
end

functor WithOrd (Outer : EXT_GENERIC) :> ORD_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Outer and Inner = Ord)
   open Ord Joined
   structure Ord = Index
   val compare = fn ? => compare (Outer.Index.getT ?)
end
