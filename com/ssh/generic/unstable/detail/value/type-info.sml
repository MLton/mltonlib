(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeInfo (Arg : OPEN_CASES) : TYPE_INFO_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   datatype t = INT of {base : Bool.t}
   datatype s = INS of {base : Bool.t, alts : Int.t}
   datatype p = INP of {base : Bool.t, elems : Int.t}

   val base = INT {base = true}
   fun pure (INT {...}) = INT {base = true}

   structure TypeInfo = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = struct
         type  'a      t = t
         type  'a      s = s
         type ('a, 'k) p = p
      end)

   open TypeInfo.This

   fun outS (INS r) = r
   fun outP (INP r) = r

   fun hasBaseCase  ? = (#base o outS o getS) ?
   fun numAlts      ? = (#alts o outS o getS) ?

   fun numElems     ? = (#elems o outP o getP) ?

   structure Layered = LayerCases
     (structure Outer = Arg and Result = TypeInfo and Rep = TypeInfo.Closed

      val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` (INP l, INP r) =
          INP {base  = #base l andalso #base r, elems = #elems l + #elems r}
      fun T (INT {base, ...}) = INP {base = base, elems = 1}
      fun R _ = T
      fun tuple (INP {base, ...}) = INT {base = base}
      val record = tuple

      fun op +` (INS l, INS r) =
          INS {alts = #alts l + #alts r, base = #base l orelse #base r}
      val unit = base
      fun C0 _ = INS {alts = 1, base = true}
      fun C1 _ (INT {base, ...}) = INS {alts = 1, base = base}
      fun data (INS {base, ...}) = INT {base = base}

      fun Y ? = Tie.pure (fn () => (INT {base = false}, id)) ?

      fun op --> _ = base

      val exn = INT {base = true}
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun array (INT {...}) = INT {base = true}
      fun refc (INT {base, ...}) = INT {base = base}

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
