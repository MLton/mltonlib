(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of a type-indexed family of compare functions.  The
 * idea is that the compare functions just implement some arbitrary
 * logical ordering that you need for things such as search trees.
 *
 * Note that comparison of functions is impossible and fails at run-time.
 * Comparison of exceptions only works when both exception constructors
 * involved in a comparison have been registered with {regExn}.  Also,
 * comparison of arrays and references does not coincide with SML's notion
 * of equality.  More precisely, for an implementation of the {COMPARE}
 * signature, two arrays (or refs) {a} and {b} may compare {EQUAL}, but it
 * is not necessarily the case that {a=b} evaluates to {true}.
 *)

signature COMPARE = sig
   type 'a compare_t

   val compare : 'a compare_t -> 'a Cmp.t
   (** Extracts the compare function. *)
end

functor LiftCompare
           (include COMPARE
            type 'a t
            val lift : ('a compare_t, 'a t) Lift.t Thunk.t) : COMPARE = struct
   type 'a compare_t = 'a t
   val compare = fn ? => Lift.get lift compare ?
end

structure Compare :> sig
   include STRUCTURAL_TYPE
   include COMPARE where type 'a compare_t = 'a t
end = struct
   type 'a t = 'a Cmp.t
   type 'a compare_t = 'a t

   val compare = id

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
   val exn : Exn.t t Ref.t = ref TypeUtil.failExnSq
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
   val word16 = Word16.compare
   val word32 = Word32.compare
   val word64 = Word64.compare
end
