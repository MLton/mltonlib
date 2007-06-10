(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TypeInfo :> TYPE_INFO_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infixr 6 <^> <+>
   infixr 5 <$> <$$> </> <//>
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   (* XXX separate datatype for sums, products, and whole indices *)

   datatype u =
      IN of {alts : Int.t,
             base : Bool.t,
             exn : Bool.t,
             pure : Bool.t,
             recs : Int.t List.t}

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

   structure Lifted = LiftGeneric
     (structure Index = struct
         type 'a t = u
         type 'a s = u
         type ('a, 'k) p = u
      end

      val base = IN {alts = 1, base = true, exn = false, pure = true, recs = []}
      fun pure (IN {exn, recs, ...}) =
          IN {alts = 1, base = true, exn = exn, pure = true, recs = recs}

      fun iso (IN {base, exn, pure, recs, ...}) =
          const (IN {alts = 1, base = base, exn = exn, pure = pure, recs = recs})

      fun (IN {base = bl, exn = hl, recs = rl, ...}) *`
          (IN {base = br, exn = hr, recs = rr, ...}) =
          IN {alts = 1, base = bl andalso br, exn = hl orelse hr, pure = true,
              recs = merge (rl, rr)}

      fun (IN {alts = al, base = bl, exn = hl, recs = rl, ...}) +`
          (IN {alts = ar, base = br, exn = hr, recs = rr, ...}) =
          IN {alts = al + ar, base = bl orelse br, exn = hl orelse hr, pure = true,
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
                       (IN {alts = 1, base = false, exn = false, pure = true, recs = [this]},
                        fn IN {alts, base, exn, pure, recs} =>
                           IN {alts = alts, base = base, exn = exn, pure = pure,
                               recs = remove this recs})
                    end) ?
      end

      fun _ --> _ = base

      val exn = IN {alts = 1, base = true, exn = true, pure = true, recs = []}
      fun regExn _ _ = ()

      fun array (IN {exn, recs, ...}) =
          IN {alts = 1, base = true, exn = exn, pure = false, recs = recs}
      fun refc (IN {base, exn, recs, ...}) =
          IN {alts = 1, base = base, exn = exn, pure = false, recs = recs}

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

   structure TypeInfo = Index

   fun out (IN t, _) = t

   fun hasBaseCase ? = (#base o out) ?
   fun hasExn ? = (#exn o out) ?
   fun hasRecData ? = (not o null o #recs o out) ?
   fun isRefOrArray ? = (not o #pure o out) ?
   fun numConsecutiveAlts ? = (#alts o out) ?
   fun canBeCyclic ? = (isRefOrArray andAlso (hasExn orElse hasRecData)) ?
end

functor WithTypeInfo (Outer : EXT_GENERIC) : TYPE_INFO_GENERIC = struct
   structure Joined = JoinGenerics (structure Outer = Outer and Inner = TypeInfo)
   open TypeInfo Joined
   structure TypeInfo = Index
   fun mk f = f o Outer.Index.getT
   val canBeCyclic        = fn ? => mk canBeCyclic        ?
   val hasExn             = fn ? => mk hasExn             ?
   val hasRecData         = fn ? => mk hasRecData         ?
   val isRefOrArray       = fn ? => mk isRefOrArray       ?
   fun mk f = f o Outer.Index.getS
   val hasBaseCase        = fn ? => mk hasBaseCase        ?
   val numConsecutiveAlts = fn ? => mk numConsecutiveAlts ?
end
