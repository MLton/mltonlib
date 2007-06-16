(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX Devise a better hash function.  This is not pretty. *)

functor WithHash (Arg : WITH_HASH_DOM) : HASH_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse >>=
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   open GenericsUtil

   structure W = Word

   type 'a t = 'a -> {maxWidth : Int.t, maxDepth : Int.t} -> Word.t UnOp.t

   structure HC : sig
      val map : ('b -> 'a) -> 'a t -> 'b t
      val withConst : Word.t -> 'a t UnOp.t
      val lift : ('a -> Word.t) -> 'a t
   end = struct
      fun map b2a hA = hA o b2a
      fun withConst w hA a p r = hA a p (W.+ (w, r))
      fun lift toWord a _ r = r * 0w19 + toWord a
   end

   structure Rep =
      JoinGenericReps
         (structure Outer = Arg.Rep
          structure Inner =
             OpenGenericRep (MkClosedGenericRep (type 'a t = 'a t)))

   structure Hash = Rep

   fun hash t v =
       Pair.fst (Arg.Rep.getT t) v {maxWidth = 200, maxDepth = 10} 0wx2CA4B13

   fun iso' bH (a2b, _) = bH o a2b

   fun iso ? = morph Arg.iso iso' ?
   fun isoProduct ? = morph Arg.isoProduct iso' ?
   fun isoSum ? = morph Arg.isoSum iso' ?

   fun op *` xy2z (aT, bT) =
       op2 Arg.*`
           (fn (aH, bH) =>
               fn a & b => fn {maxWidth, maxDepth} => let
                  val aN = Arg.numElems aT
                  val bN = Arg.numElems bT
                  val aW = Int.quot (maxWidth * aN, aN + bN)
                  val bW = maxWidth - aW
               in
                  bH b {maxWidth = bW, maxDepth = maxDepth} o
                  aH a {maxWidth = aW, maxDepth = maxDepth}
               end)
           xy2z (aT, bT)

   fun op +` ? =
       op2 Arg.+`
           (Sum.sum o
            Pair.map (HC.withConst 0wx96BA232,
                      HC.withConst 0wxCF2465)) ?

   fun Y ? = y Arg.Y Tie.function ?

   fun op --> ? = op2 Arg.--> (fn _ => failing "Hash.--> unsupported") ?

   fun exn ? = op0 Arg.exn (failing "Hash.exn unsupported") ?
   fun regExn ? = re Arg.regExn (const ignore) ?

   fun refc ? = op1 Arg.refc (HC.withConst 0wx178A2346 o HC.map !) ?

   fun list ? =
       op1 Arg.list
           (fn hX => fn xs => fn {maxWidth, maxDepth} => fn h => let
               val m = Int.quot (maxWidth, 2)
               fun len n []      = n
                 | len n (_::xs) = if m <= n then n else len (n+1) xs
               val n = len 0 xs
               val p = {maxWidth = Int.quot (maxWidth, n),
                        maxDepth = maxDepth - 1}
               fun lp h _ []      = h
                 | lp h n (x::xs) = if n = 0 then h else lp (hX x p h) (n-1) xs
            in
               lp h n xs
            end) ?

   fun hashSeq length sub hashElem s {maxWidth, maxDepth} h = let
      val n = length s
   in
      case Int.min (Int.quot (maxWidth+3, 4), Int.quot (n+1, 2)) of
         0          => h
       | numSamples => let
            val p = {maxWidth = Int.quot (maxWidth, numSamples),
                     maxDepth = maxDepth - 1}
            fun lp h 0 = h
              | lp h n = lp (hashElem (sub (s, n-1)) p h) (n-1)
         in
            lp h (Int.max (numSamples, Int.min (10, n)))
         end
   end

   fun array ? = op1 Arg.array (hashSeq Array.length Array.sub) ?
   fun vector ? = op1 Arg.vector (hashSeq Vector.length Vector.sub) ?

   val char' = HC.lift (Word.fromInt o ord)
   fun char ? = op0 Arg.char char' ?

   val string' = hashSeq String.length String.sub char'
   fun string ? = op0 Arg.string string' ?

   val unit' = HC.lift (Thunk.mk 0wx2F785)
   fun unit ? = op0 Arg.unit unit' ?

   local
      fun mk outer toWord ? = op0 outer (HC.lift toWord) ?
   in
      fun largeInt ? =
          mk Arg.largeInt
             (W.fromLargeInt o LargeInt.rem /> W.toLargeInt W.maxValue) ?
      fun largeWord ? =
          mk Arg.largeWord
             (W.fromLarge o LargeWord.mod /> W.toLarge W.maxValue) ?
      fun word8 ? = mk Arg.word8 Word8.toWord ?
   (* fun word16 ? = mk Arg.word16 Word16.toWord ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = mk Arg.word32 (Word.fromLarge o Word32.toLarge) ?
      fun word64 ? = mk Arg.word64 (Word.fromLarge o Word64.toLarge) ?
      fun bool ? = mk Arg.bool (fn true => 0wx2DA745 | false => 0wx3C24A62) ?
      fun int ? = mk Arg.int Word.fromInt ?
      fun word ? = mk Arg.word id ?
   end

   (* XXX SML/NJ does not provide a function to convert a real to bits *)
   fun largeReal ? = op0 Arg.largeReal (HC.map LargeReal.toString string') ?
   fun real ? = op0 Arg.real (HC.map Real.toString string') ?

   (* Trivialities *)

   fun T ? = t Arg.T id ?
   fun R ? = r Arg.R (const id) ?

   local
      fun width h : 'a t =
          fn a => fn p => if #maxWidth p = 0 then id else h a p
   in
      fun tuple ? = op1 Arg.tuple width ?
      fun record ? = op1 Arg.record width ?
   end

   fun C0 ? = c0 Arg.C0 (const unit') ?
   fun C1 ? = c1 Arg.C1 (const id) ?
   fun data ? =
       op1 Arg.data
           (fn h => fn a => fn {maxDepth, maxWidth} =>
               if maxDepth = 0 then id
               else h a {maxDepth = maxDepth - 1,
                         maxWidth = Int.quot (maxWidth, 2)}) ?
end
