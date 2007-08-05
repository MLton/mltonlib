(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithHash (Arg : WITH_HASH_DOM) : HASH_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   type 'a t = 'a -> {totWidth : Int.t, maxDepth : Int.t} -> Word.t

   fun prim f : 'a t = const o f

   fun viaWord x2V op mod (v2w, w2v) =
       prim (fn x => v2w (x2V x mod w2v Word.largestPrime))

   fun iso' bH (a2b, _) = bH o a2b

   structure Hash =
      LayerGenericRep (structure Outer = Arg.Rep
                       structure Closed = MkClosedRep (type 'a t = 'a t))

   open Hash.This

   fun hashParam t p =
       if #totWidth p < 0 orelse #maxDepth p < 0
       then raise Domain
       else fn v => getT t v p

   fun hash t = hashParam t {totWidth = 200, maxDepth = 10}

   structure Layered = LayerDepGeneric
     (structure Outer = Arg and Result = Hash

      fun iso        ? = iso' (getT ?)
      fun isoProduct ? = iso' (getP ?)
      fun isoSum     ? = iso' (getS ?)

      fun op *` (aT, bT) (a & b) {totWidth, maxDepth} = let
         val aN = Arg.numElems aT
         val bN = Arg.numElems bT
         val aW = Int.quot (totWidth * aN, aN + bN)
         val bW = totWidth - aW
      in
         getP bT b {totWidth = bW, maxDepth = maxDepth} * 0w13 +
         getP aT a {totWidth = aW, maxDepth = maxDepth}
      end
      val T   = getT
      fun R _ = getT
      fun tuple aP a p = if #totWidth p = 0 then 0w0 else getP aP a p
      val record = tuple

      fun op +` ? = let
         fun withConst c f v p = Word.xorb (f v p, c)
      in
         Sum.sum o Pair.map (withConst 0wx96BA232 o getS,
                             withConst 0wxCF24651 o getS)
      end ?
      val unit = prim (Thunk.mk 0wx2F785)
      fun C0 _ = unit
      fun C1 _ = getT
      fun data aS a {maxDepth, totWidth} =
          if maxDepth = 0 then 0w0
          else getS aS a {maxDepth = maxDepth - 1,
                          totWidth = totWidth}

      val Y = Tie.function

      fun op --> _ = failing "Hash.--> unsupported"

      fun refc aT = getT aT o !

      val int = prim Word.fromInt

      fun list xT xs {totWidth, maxDepth} = let
         val m = Int.quot (totWidth, 2)
         fun len n []      = n
           | len n (_::xs) = if m <= n then n else len (n+1) xs
      in
         case len 0 xs of
            0 => 0wx2A4C7A
          | n => let
               val p = {totWidth = Int.quot (totWidth, n),
                        maxDepth = maxDepth - 1}
               fun lp h _ []      = h
                 | lp h n (x::xs) =
                   if n = 0 then h else lp (h * 0w17 + getT xT x p) (n-1) xs
            in
               lp (Word.fromInt n) n xs
            end
      end

      fun hashSeq length sub hashElem s {totWidth, maxDepth} = let
         val n = length s
         val h = Word.fromInt n
      in
         case Int.min (Int.quot (totWidth+3, 4), Int.quot (n+1, 2)) of
            0          => h
          | numSamples => let
               val p = {totWidth = Int.quot (totWidth, numSamples),
                        maxDepth = maxDepth - 1}
               fun lp h 0 = h
                 | lp h n = lp (h * 0w19 + hashElem (sub (s, n-1)) p) (n-1)
            in
               lp h (Int.max (numSamples, Int.min (10, n)))
            end
      end

      fun array  aT = hashSeq Array.length  Array.sub  (getT aT)
      fun vector aT = hashSeq Vector.length Vector.sub (getT aT)

      val char = prim (Word.fromInt o ord)
      val string = hashSeq String.length String.sub char

      val exn = string o Exn.message (* XXX Imprecise *)
      fun regExn _ _ = ()

      val bool = prim (fn true => 0wx2DA745 | false => 0wx3C24A62)
      val real =
          let open CastReal in viaWord (#1 isoBits) op mod Bits.isoWord end
      val word = const

      val largeInt  = viaWord id op mod (Iso.swap Word.isoLargeInt)
      val largeReal =
          let open CastLargeReal in viaWord (#1 isoBits) op mod Bits.isoWord end
      val largeWord = viaWord id op mod LargeWord.isoWord

      val word8  = prim Word8.toWord
      val word32 = prim Word32.toWord
      val word64 = viaWord id op mod Word64.isoWord)

   open Layered
end
