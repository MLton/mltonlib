(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX UNFINISHED!  NOTE THE USES OF `undefined` BELOW. *)

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

   structure W = Word

   fun mac word hash =
       hash * 0w19 + word

   datatype 'a t =
     IN of 'a -> {maxWidth : Int.t, maxDepth : Int.t} -> Word.t UnOp.t

   structure Rep =
      JoinGenericReps
         (structure Outer = Arg.Rep
          structure Inner =
             OpenGenericRep (MkClosedGenericRep (type 'a t = 'a t)))

   structure Hash = Rep

   fun hash t v =
       case Pair.fst (Arg.Rep.getT t) of
          IN h => h v {maxWidth = 200, maxDepth = 10} 0wx2CA4B13

   fun iso' (IN bH) (a2b, _) = IN (bH o a2b)

   fun morph outer f = outer (fn (a, x) => fn i => (iso' a i, f x i))
   fun nullary outer t x = outer (t, x)
   fun bop outer f g = outer (Pair.map (f, g) o Pair.swizzle)
   fun uop outer f g = outer (Pair.map (f, g))

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?

   fun op *` xy2z (aT, bT) =
       bop Arg.*`
           (fn (IN aH, IN bH) =>
               IN (fn a & b => fn {maxWidth, maxDepth} => let
                      val aN = Arg.numElems aT
                      val bN = Arg.numElems bT
                      val aW = Int.quot (maxWidth * aN, aN + bN)
                      val bW = maxWidth - aW
                   in
                      bH b {maxWidth = bW, maxDepth = maxDepth} o
                      aH a {maxWidth = aW, maxDepth = maxDepth}
                   end))
           xy2z (aT, bT)

   fun op +` ? =
       bop Arg.+`
           (fn (IN aH, IN bH) =>
               IN (Sum.sum (aH, bH))) ?

   fun Y y = Arg.Y (let open Tie in iso (function *` y) end
                       (fn (IN a, b) => a & b,
                        fn a & b => (IN a, b)))

   fun op --> ? = bop Arg.--> (fn _ => IN (failing "Hash.--> unsupported")) ?

   fun exn ? = let
      val e = Fail "Hash.exn unsupported"
   in
      nullary Arg.exn (IN (raising e))
   end ?
   fun regExn ef = Arg.regExn (ef o Pair.snd)

   fun refc ? =
       uop Arg.refc
           (fn IN aH => IN (fn a => fn p => mac 0w87 o aH (!a) p)) ?

   fun list ? = uop Arg.list (fn _ => IN undefined) ?

   fun array ? = uop Arg.array (fn _ => IN undefined) ?
   fun vector ? = uop Arg.vector (fn _ => IN undefined) ?

   fun string ? = nullary Arg.string (IN undefined) ?

   val unit' = IN (fn () => fn _ => mac 0w75)
   fun unit ? = nullary Arg.unit unit' ?

   local
      fun mk outer toWord ? =
          nullary outer (IN (fn x => fn _ => mac (toWord x))) ?
   in
      fun largeInt ? =
          mk Arg.largeInt
             (W.fromLargeInt o LargeInt.rem /> W.toLargeInt W.maxValue) ?
      fun largeReal ? = mk Arg.largeReal undefined ?
      fun largeWord ? =
          mk Arg.largeWord
             (W.fromLarge o LargeWord.mod /> W.toLarge W.maxValue) ?
      fun word8 ? = mk Arg.word8 Word8.toWord ?
   (* fun word16 ? = mk Arg.word16 Word16.toWord ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = mk Arg.word32 (Word.fromLarge o Word32.toLarge) ?
      fun word64 ? = mk Arg.word64 (Word.fromLarge o Word64.toLarge) ?
      fun bool ? = mk Arg.bool (fn true => 0wx2DA745 | false => 0wx3C24A62) ?
      fun char ? = mk Arg.char (Word.fromInt o ord) ?
      fun int ? = mk Arg.int Word.fromInt ?
      fun real ? = mk Arg.real undefined ?
      fun word ? = mk Arg.word id ?
   end

   (* Trivialities *)

   fun T ? = uop Arg.T id ?
   fun R f = Arg.R (fn l => Pair.map (id, f l))

   local
      fun width (IN h) =
          IN (fn a => fn p => if #maxWidth p = 0 then id else h a p)
   in
      fun tuple ? = uop Arg.tuple width ?
      fun record ? = uop Arg.record width ?
   end

   fun C0 f = Arg.C0 (fn l => (unit', f l))
   fun C1 f = Arg.C1 (fn l => Pair.map (id, f l))
   fun data ? =
       uop Arg.data
           (fn IN h => IN (fn a => fn {maxDepth, maxWidth} =>
               if maxDepth = 0 then id
               else h a {maxDepth = maxDepth-1,
                         maxWidth = Int.quot (maxWidth, 2)})) ?
end
