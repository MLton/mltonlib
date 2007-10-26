(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithReduce (Arg : WITH_REDUCE_DOM) : REDUCE_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   fun sequ toSlice getItem xR (z, p, xs) = let
      fun lp (s, xs) =
          case getItem xs
           of NONE         => s
            | SOME (x, xs) => lp (p (s, xR (z, p, x)), xs)
   in
      case getItem (toSlice xs)
       of NONE         => z
        | SOME (x, xs) => lp (xR (z, p, x), xs)
   end

   fun default (z, _, _) = z

   structure ReduceRep = LayerRep
     (open Arg
      structure Rep = MkClosedRep
        (type 'a t = Univ.t * Univ.t BinOp.t * 'a -> Univ.t))

   open ReduceRep.This

   fun makeReduce z p a2r aT aT2bT = let
      val (to, from) = Univ.Iso.new ()
      val z = to z
      val p = BinOp.map (from, to) p
      val aT = mapT (const (to o a2r o #3)) aT
      val bR = getT (aT2bT aT)
   in
      fn x => from (bR (z, p, x))
   end

   structure Open = LayerCases
     (fun iso bR (a2b, _) (z, p, a) = bR (z, p, a2b a)
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (aR, bR) (z, p, (a & b)) =
          p (aR (z, p, a), bR (z, p, b))
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (aR, bR) =
       fn (z, p, INL a) => aR (z, p, a)
        | (z, p, INR b) => bR (z, p, b)
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Reduce.--> has no default"

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list   ? = sequ             id          List.getItem ?
      fun vector ? = sequ VectorSlice.full VectorSlice.getItem ?
      fun array  ? = sequ  ArraySlice.full  ArraySlice.getItem ?

      fun refc aR (z, p, r) = aR (z, p, !r)

      val fixedInt = default
      val largeInt = default

      val largeReal = default
      val largeWord = default

      val bool   = default
      val char   = default
      val int    = default
      val real   = default
      val string = default
      val word   = default

      val word8  = default
      val word32 = default
(*
      val word64 = default
*)

      fun hole () = undefined

      open Arg ReduceRep)
end
