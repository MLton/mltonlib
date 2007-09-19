(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithDataRecInfo (Arg : OPEN_CASES) : DATA_REC_INFO_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  2 andAlso
   infix  1 orElse
   (* SML/NJ workaround --> *)

   type recs = Unit.t Ref.t List.t

   fun rem x : recs UnOp.t =
    fn []  => []
     | [y] => if x = y then [] else [y]
     | ys  => List.filter (notEq x) ys

   val merge : recs BinOp.t =
    fn ([], ys)   => ys
     | (xs, [])   => xs
     | ([x], [y]) => if x = y then [x] else [x, y]
     | (xs, ys)   =>
       foldl (fn (x, ys) => if List.exists (eq x) ys then ys else x::ys) ys xs

   datatype t = INT of {exn : Bool.t, recs : recs, pure : Bool.t}
   datatype s = INS of {exn : Bool.t, recs : recs}
   datatype p = INP of {exn : Bool.t, recs : recs}

   val base = INT {exn = false, pure = true, recs = []}
   fun pure (INT {exn, recs, ...}) = INT {exn = exn, pure = true, recs = recs}
   fun mutable (INT {exn, recs, ...}) =
       INT {exn = exn, pure = false, recs = recs}

   structure DataRecInfoRep = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = struct
         type  'a      t = t
         type  'a      s = s
         type ('a, 'k) p = p
      end)

   open DataRecInfoRep.This

   fun outT (INT r) = r

   fun mayContainExn ? = (#exn o outT o getT) ?
   fun mayBeRecData  ? = (not o null o #recs o outT o getT) ?
   fun isMutableType ? = (not o #pure o outT o getT) ?
   fun mayBeCyclic   ? =
       (isMutableType andAlso (mayContainExn orElse mayBeRecData)) ?

   structure Layered = LayerCases
     (structure Outer=Arg and Result=DataRecInfoRep
         and Rep=DataRecInfoRep.Closed

      val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` (INP l, INP r) =
          INP {exn = #exn l orelse #exn r, recs = merge (#recs l, #recs r)}
      fun T (INT {exn, recs, ...}) = INP {exn = exn, recs = recs}
      fun R _ = T
      fun tuple (INP {exn, recs, ...}) =
          INT {exn = exn, pure = true, recs = recs}
      val record = tuple

      fun op +` (INS l, INS r) =
          INS {exn = #exn l orelse #exn r, recs = merge (#recs l, #recs r)}
      val unit = base
      fun C0 _ = INS {exn = false, recs = []}
      fun C1 _ (INT {exn, recs, ...}) = INS {exn = exn, recs = recs}
      fun data (INS {exn, recs, ...}) =
          INT {exn = exn, pure = true, recs = recs}

      val Y = Tie.pure
                 (fn () => let
                        val me = ref ()
                     in
                        (INT {exn = false, pure = true, recs = [me]},
                         fn INT {exn, pure, recs} =>
                            INT {exn = exn, pure = pure, recs = rem me recs})
                     end)

      fun op --> _ = base

      val exn = INT {exn = true, pure = true, recs = []}
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      val array = mutable
      val refc  = mutable

      val vector = pure
      val list   = pure

      val fixedInt = base
      val largeInt = base

      val largeReal = base
      val largeWord = base

      val bool   = base
      val char   = base
      val int    = base
      val real   = base
      val string = base
      val word   = base

      val word8  = base
      val word32 = base
      val word64 = base)

   open Layered
end
