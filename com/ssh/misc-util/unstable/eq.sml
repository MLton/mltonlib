(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of a type-indexed equality relation.  For equality
 * types the semantics is the same as SML's built-in equality.  User
 * defined types, exceptions, and reals are given a natural, structural,
 * semantics of equality.  Functions, obviously, can't be supported.
 *)

signature EQ = sig
   type 'a eq_t

   val eq : 'a eq_t -> 'a BinPr.t
   (**
    * Extracs the equality relation.  Note that the type parameter {'a}
    * isn't an equality type variable.
    *)

   val notEq : 'a eq_t -> 'a BinPr.t
   (** {notEq t = not o eq t} *)
end

functor LiftEq
           (include EQ
            type 'a t
            val lift : ('a eq_t, 'a t) Lift.t Thunk.t) : EQ = struct
   type 'a eq_t = 'a t
   val eq    = fn ? => Lift.get lift eq    ?
   val notEq = fn ? => Lift.get lift notEq ?
end

structure Eq :> sig
   include STRUCTURAL_TYPE
   include EQ where type 'a eq_t = 'a t
end = struct
   type 'a t = 'a BinPr.t
   type 'a eq_t = 'a t

   val eq = id
   val notEq = negate

   fun iso b (a2b, _) = b o Pair.map (Sq.mk a2b)

   val op *` = Product.equal
   val op +` = Sum.equal

   val Y = Tie.function

   local
      val e = Fail "Eq.--> not supported"
   in
      fun _ --> _ = raising e
   end

   val exn : Exn.t t Ref.t = ref TypeUtil.failExnSq
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
   val word16 = op =
   val word32 = op =
   val word64 = op =
end
