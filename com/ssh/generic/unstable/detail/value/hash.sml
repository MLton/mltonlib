(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithHash (Arg : WITH_HASH_DOM) : HASH_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   infix  0 &
   (* SML/NJ workaround --> *)

   type p = {totWidth : Int.t, maxDepth : Int.t}
   type 'a t = 'a * p -> Word.t

   fun prim f : 'a t = f o #1

   fun viaWord x2V op mod (v2w, w2v) =
       prim (fn x => v2w (x2V x mod w2v Word.largestPrime))

   fun iso' bH (a2b, _) = bH o Pair.map (a2b, id)

   fun sequ length sub hashElem (s, {totWidth, maxDepth}) = let
      val n = length s
      val h = Word.fromInt n
   in
      case Int.min (Int.quot (totWidth+3, 4), Int.quot (n+1, 2))
       of 0          => h
        | numSamples => let
             val p = {totWidth = Int.quot (totWidth, numSamples),
                      maxDepth = maxDepth}
             fun lp (h, 0) = h
               | lp (h, n) = lp (h * 0w19 + hashElem (sub (s, n-1), p), n-1)
          in
             lp (h, Int.max (numSamples, Int.min (10, n)))
          end
   end

   val exns : (Exn.t * p -> Word.t Option.t) Buffer.t = Buffer.new ()

   structure Hash = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Hash.This

   val defaultHashParam = {totWidth = 200, maxDepth = 10}

   fun hashParam t = let
      val h = getT t
      val th = Word32.toWord (Arg.typeHash t)
   in
      fn p =>
         if #totWidth p < 0 orelse #maxDepth p < 0
         then raise Domain
         else th <\ Word.xorb o h /> p
   end

   fun hash t = hashParam t defaultHashParam

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Hash

      fun iso        ? = iso' (getT ?)
      fun isoProduct ? = iso' (getP ?)
      fun isoSum     ? = iso' (getS ?)

      fun op *` (aT, bT) = let
         val aN = Arg.numElems aT
         val bN = Arg.numElems bT
         val aH = getP aT
         val bH = getP bT
      in
         fn (a & b, {totWidth, maxDepth}) => let
               val aW = Int.quot (totWidth * aN, aN + bN)
               val bW = totWidth - aW
            in
               bH (b, {totWidth = bW, maxDepth = maxDepth}) * 0w13 +
               aH (a, {totWidth = aW, maxDepth = maxDepth})
            end
      end
      val T   = getT
      fun R _ = getT
      fun tuple aP =
          case getP aP
           of aH => fn (a, p) =>
                       if #totWidth p = 0 then 0wx65B2531B else aH (a, p)
      val record = tuple

      fun op +` (aS, bS) = let
         val aH = getS aS
         val bH = getS bS
      in
         fn (INL a, p) => Word.xorb (0wx04D55ADB, aH (a, p))
          | (INR b, p) => Word.xorb (0wx05B6D5A3, bH (b, p))
      end
      val unit = prim (Thunk.mk 0wx062DAD9B)
      fun C0 _ = unit
      fun C1 _ = getT
      fun data aS = let
         val aH = getS aS
      in
         fn (a, {maxDepth, totWidth}) =>
            if maxDepth = 0 then 0wx36958B65
            else aH (a, {maxDepth = maxDepth - 1, totWidth = totWidth})
      end

      val Y = Tie.function

      fun op --> _ = failing "Hash.--> unsupported"

      fun refc aT = getT aT o Pair.map (!, id)

      val int = prim Word.fromInt

      fun list xT = let
         val xH = getT xT
      in
         fn (xs, {totWidth, maxDepth}) => let
               val m = Int.quot (totWidth, 2)
               fun len (n,    []) = n
                 | len (n, _::xs) = if m <= n then n else len (n+1, xs)
            in
               case len (0, xs)
                of 0 => 0wx2A4C5ADB
                 | n => let
                      val p = {totWidth = Int.quot (totWidth, n),
                               maxDepth = maxDepth}
                      fun lp (h, _,    []) = h
                        | lp (h, n, x::xs) =
                          if n = 0
                          then h
                          else lp (h * 0w17 + xH (x, p), n-1, xs)
                   in
                      lp (Word.fromInt n, n, xs)
                   end
            end
      end

      fun array  aT = sequ Array.length  Array.sub  (getT aT)
      fun vector aT = sequ Vector.length Vector.sub (getT aT)

      val char = prim (Word.fromInt o ord)
      val string = sequ String.length String.sub char

      fun exn (e, {maxDepth, totWidth}) =
          if maxDepth = 0 then 0wx1A35B599
          else case Buffer.findSome (pass (e, {maxDepth = maxDepth - 1,
                                               totWidth = totWidth})) exns
                of NONE   => GenericsUtil.failExn e
                 | SOME h => h
      fun regExn0 c (_, e2t) =
          case string (Generics.Con.toString c, defaultHashParam)
           of c => (Buffer.push exns)
                      (fn (e, _) => if isSome (e2t e) then SOME c else NONE)
      fun regExn1 c t (_, e2t) =
          case string (Generics.Con.toString c, defaultHashParam) & getT t
           of c & t => (Buffer.push exns)
                          (fn (e, p) =>
                              case e2t e
                               of NONE   => NONE
                                | SOME v => SOME (Word.xorb (c, t (v, p))))

      val bool = prim (fn true => 0wx096DB16D | false => 0wx01B56B6D)
      val real =
          let open CastReal in viaWord (#1 isoBits) op mod Bits.isoWord end
      val word = prim id

      val fixedInt = viaWord id op mod (Iso.swap Word.isoFixedInt)
      val largeInt = viaWord id op mod (Iso.swap Word.isoLargeInt)

      val largeReal =
          let open CastLargeReal in viaWord (#1 isoBits) op mod Bits.isoWord end
      val largeWord = viaWord id op mod LargeWord.isoWord

      val word8  = prim Word8.toWord
      val word32 = prim Word32.toWord
      val word64 = viaWord id op mod Word64.isoWord)

   open Layered
end
