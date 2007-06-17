(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeInfo (Arg : OPEN_GENERIC) : TYPE_INFO_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   fun revMerge (xs, ys) = let
      fun lp ([], ys, zs) = (ys, zs)
        | lp (xs, [], zs) = (xs, zs)
        | lp (x::xs, y::ys, zs) =
          case Int.compare (x, y) of
             LESS => lp (xs, y::ys, x::zs)
           | EQUAL => lp (xs, ys, x::zs)
           | GREATER => lp (x::xs, ys, y::zs)
   in
      lp (xs, ys, [])
   end

   val merge = List.revAppend o Pair.swap o revMerge

   fun remove x ys = let
      fun lp (zs, []) = (zs, [])
        | lp (zs, y::ys) =
          case Int.compare (x, y) of
             LESS => (zs, y::ys)
           | EQUAL => (zs, ys)
           | GREATER => lp (y::zs, ys)
   in
      List.revAppend (lp ([], ys))
   end

   datatype t =
      INT of {base : Bool.t,
              exn : Bool.t,
              pure : Bool.t,
              recs : Int.t List.t}

   datatype s =
      INS of {alts : Int.t,
              base : Bool.t,
              exn : Bool.t,
              recs : Int.t List.t}

   datatype p =
      INP of {base : Bool.t,
              elems : Int.t,
              exn : Bool.t,
              recs : Int.t List.t}

   structure TypeInfo =
      LayerGenericRep
        (structure Outer = Arg.Rep
         structure Closed = struct
            type 'a t = t
            type 'a s = s
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

      val iso = const
      val isoProduct = const
      val isoSum = const

      fun (INP {base = bl, elems = el, exn = hl, recs = rl, ...}) *`
          (INP {base = br, elems = er, exn = hr, recs = rr, ...}) =
          INP {base = bl andalso br, elems = el + er, exn = hl orelse hr,
               recs = merge (rl, rr)}

      fun (INS {alts = al, base = bl, exn = hl, recs = rl, ...}) +`
          (INS {alts = ar, base = br, exn = hr, recs = rr, ...}) =
          INS {alts = al + ar, base = bl orelse br, exn = hl orelse hr,
               recs = merge (rl, rr)}

      val unit = base

      local
         val id = ref 0
      in
         fun Y ? =
             Tie.pure
                (fn () => let
                       val this = !id before id := !id + 1
                    in
                       (INT {base = false, exn = false, pure = true,
                             recs = [this]},
                        fn INT {base, exn, pure, recs} =>
                           INT {base = base, exn = exn, pure = pure,
                               recs = remove this recs})
                    end) ?
      end

      fun _ --> _ = base

      val exn = INT {base = true, exn = true, pure = true, recs = []}
      fun regExn _ _ = ()

      fun array (INT {exn, recs, ...}) =
          INT {base = true, exn = exn, pure = false, recs = recs}
      fun refc (INT {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = false, recs = recs}

      val vector = pure

      val largeInt  = base
      val largeReal = base
      val largeWord = base

      val list = pure

      val bool   = base
      val char   = base
      val int    = base
      val real   = base
      val string = base
      val word   = base

      val word8  = base
   (* val word16 = base (* Word16 not provided by SML/NJ *) *)
      val word32 = base
      val word64 = base

      (* Trivialities *)

      fun T (INT {base, exn, recs, ...}) =
          INP {base = base, elems = 1, exn = exn, recs = recs}
      fun R _ = T

      fun tuple (INP {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = true, recs = recs}
      val record = tuple

      fun C0 _ = INS {alts = 1, base = true, exn = false, recs = []}
      fun C1 _ (INT {base, exn, recs, ...}) =
          INS {alts = 1, base = base, exn = exn, recs = recs}
      fun data (INS {base, exn, recs, ...}) =
          INT {base = base, exn = exn, pure = true, recs = recs})

   open Layered
end
