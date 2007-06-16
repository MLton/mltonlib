(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  0 &
   (* SML/NJ workaround --> *)

   structure Ord : CLOSED_GENERIC = struct
      structure Rep = struct
         type 'a t = 'a Cmp.t
         type 'a s = 'a t
         type ('a, 'k) p = 'a t
      end

      fun inj b a2b = b o Pair.map (Sq.mk a2b)
      fun iso b = inj b o Iso.to

      val op *` = Product.collate
      val op +` = Sum.collate

      val Y = Tie.function

      fun op --> _ = failing "Compare.--> unsupported"

     (* XXX It is also possible to implement exn so that compare provides
      * a reasonable answer as long as at least one of the exception
      * variants (involved in a comparison) has been registered.
      *)
      val exn : Exn.t Rep.t Ref.t = ref GenericsUtil.failExnSq
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
   (* val word16 = Word16.compare (* Word16 not provided by SML/NJ *) *)
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
      val data = id
   end

   structure Ord : OPEN_GENERIC = OpenGeneric (Ord)
in
   structure Ord :> ORD_GENERIC = struct
      open Ord
      structure Ord = Rep
      val compare : ('a, 'x) Ord.t -> 'a Cmp.t = Pair.fst
   end
end

functor WithOrd (Arg : OPEN_GENERIC) : ORD_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Arg and Inner = Ord)
   open Ord Joined
   structure Ord = Rep
   val compare = fn ? => compare (Arg.Rep.getT ?)
end
