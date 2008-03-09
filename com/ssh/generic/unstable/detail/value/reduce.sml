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

   datatype 'a t = IN of Univ.t * Univ.t BinOp.t * 'a -> Univ.t

   fun sequ (Ops.S {toSlice, getItem, ...}) (IN xR) =
       IN (fn (z, p, xs) => let
                 fun lp (s, xs) =
                     case getItem xs
                      of NONE         => s
                       | SOME (x, xs) => lp (p (s, xR (z, p, x)), xs)
              in
                 case getItem (toSlice xs)
                  of NONE         => z
                   | SOME (x, xs) => lp (xR (z, p, x), xs)
              end)

   val default = IN (fn (z, _, _) => z)

   structure ReduceRep = LayerRep' (open Arg type 'a t = 'a t)

   open ReduceRep.This

   fun makeReduce aT2bT aT z p a2r = let
      val (to, from) = Univ.Iso.new ()
      val z = to z
      val p = BinOp.map (from, to) p
      val aT = mapT (const (IN (to o a2r o #3))) aT
      val IN bR = getT (aT2bT aT)
   in
      fn x => from (bR (z, p, x))
   end

   structure Open = LayerCases
     (fun iso (IN bR) (a2b, _) = IN (fn (z, p, a) => bR (z, p, a2b a))
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (IN aR, IN bR) =
          IN (fn (z, p, (a & b)) => p (aR (z, p, a), bR (z, p, b)))
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (IN aR, IN bR) =
          IN (fn (z, p, INL a) => aR (z, p, a)
               | (z, p, INR b) => bR (z, p, b))
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      fun Y ? = let open Tie in iso function end (fn IN ? => ?, IN) ?

      fun op --> _ = IN (failing "Reduce.--> has no default")

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list   ? = sequ   ListOps.ops ?
      fun vector ? = sequ VectorOps.ops ?
      fun array  ? = sequ  ArrayOps.ops ?

      fun refc (IN aR) = IN (fn (z, p, r) => aR (z, p, !r))

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

      fun hole () = IN undefined

      open Arg ReduceRep)
end
