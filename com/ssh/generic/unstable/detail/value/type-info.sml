(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeInfo (Arg : OPEN_GENERIC) : TYPE_INFO_GENERIC = struct
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

   datatype t = INT of {base : Bool.t, exn : Bool.t, recs : recs, pure : Bool.t}
   datatype s = INS of {base : Bool.t, exn : Bool.t, recs : recs, alts : Int.t}
   datatype p = INP of {base : Bool.t, exn : Bool.t, recs : recs, elems : Int.t}

   structure TypeInfo =
      LayerGenericRep
        (structure Outer = Arg.Rep
         structure Closed = struct
            type  'a      t = t
            type  'a      s = s
            type ('a, 'k) p = p
         end)

   open TypeInfo.This

   fun outT (INT r) = r
   fun outS (INS r) = r
   fun outP (INP r) = r

   fun hasExn       ? = (#exn o outT o getT) ?
   fun hasRecData   ? = (not o null o #recs o outT o getT) ?
   fun isRefOrArray ? = (not o #pure o outT o getT) ?
   fun canBeCyclic  ? = (isRefOrArray andAlso (hasExn orElse hasRecData)) ?

   fun hasBaseCase  ? = (#base o outS o getS) ?
   fun numAlts      ? = (#alts o outS o getS) ?

   fun numElems     ? = (#elems o outP o getP) ?

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = TypeInfo and Rep = TypeInfo.Closed

      val base = INT {base = true, exn = false, pure = true, recs = []}
      fun pure (INT {exn, recs, ...}) =
          INT {base = true, exn = exn, pure = true, recs = recs}

      val iso        = const
      val isoProduct = const
      val isoSum     = const

      fun op *` (INP l, INP r) =
          INP {base  = #base l andalso #base r,
               elems = #elems l + #elems r,
               exn   = #exn l orelse #exn r,
               recs  = merge (#recs l, #recs r)}
      fun T (INT {base, exn, recs, ...}) =
          INP {base = base, elems = 1, exn = exn, recs = recs}
      fun R _ = T
      fun tuple (INP {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = true, recs = recs}
      val record = tuple

      fun op +` (INS l, INS r) =
          INS {alts = #alts l + #alts r,
               base = #base l orelse #base r,
               exn  = #exn l orelse #exn r,
               recs = merge (#recs l, #recs r)}
      val unit = base
      fun C0 _ = INS {alts = 1, base = true, exn = false, recs = []}
      fun C1 _ (INT {base, exn, recs, ...}) =
          INS {alts = 1, base = base, exn = exn, recs = recs}
      fun data (INS {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = true, recs = recs}

      fun Y ? =
          Tie.pure
             (fn () => let
                    val me = ref ()
                 in
                    (INT {base=false, exn=false, pure=true, recs=[me]},
                     fn INT {base, exn, pure, recs} =>
                        INT {base=base, exn=exn, pure=pure, recs=rem me recs})
                 end) ?

      fun op --> _ = base

      val exn = INT {base = true, exn = true, pure = true, recs = []}
      fun regExn _ _ = ()

      fun array (INT {exn, recs, ...}) =
          INT {base = true, exn = exn, pure = false, recs = recs}
      fun refc (INT {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = false, recs = recs}

      val vector = pure
      val list   = pure

      val largeInt  = base
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
