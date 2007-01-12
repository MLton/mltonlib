(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A functor for combining implementations of the {TYPE} signature.
 *)

functor TypePair
           (structure A : TYPE
            structure B : TYPE) : TYPE = struct
   type 'a t = 'a A.t * 'a B.t
   type 'a s = 'a A.s * 'a B.s
   type ('a, 'b) p = ('a, 'b) A.p * ('a, 'b) B.p

   local
      fun mk aIso bIso (a, b) i = (aIso a i, bIso b i)
   in
      fun iso        ? = mk A.iso        B.iso        ?
      fun isoProduct ? = mk A.isoProduct B.isoProduct ?
      fun isoSum     ? = mk A.isoSum     B.isoSum     ?
   end

   local
      fun mk t = Pair.map t o Pair.swizzle
   in
      fun op *`  ? = mk (A.*`,  B.*`)  ?
      fun op +`  ? = mk (A.+`,  B.+`)  ?
      fun op --> ? = mk (A.-->, B.-->) ?
   end

   fun T ? = Pair.map (A.T, B.T) ?
   fun R ? = Pair.map (A.R ?, B.R ?)

   fun C0 ? = (A.C0 ?, B.C0 ?)
   fun C1 ? = Pair.map (A.C1 ?, B.C1 ?)

   fun Y ? = Tie.tuple2 (A.Y, B.Y) ?

   val exn = (A.exn, B.exn)
   fun regExn (a, b) emb = (A.regExn a emb ; B.regExn b emb)

   fun tuple  ? = Pair.map (A.tuple,  B.tuple)  ?
   fun record ? = Pair.map (A.record, B.record) ?
   fun data   ? = Pair.map (A.data,   B.data)   ?

   fun array ? = Pair.map (A.array, B.array) ?
   fun refc  ? = Pair.map (A.refc,  B.refc)  ?

   fun vector ? = Pair.map (A.vector, B.vector) ?

   fun list ? = Pair.map (A.list, B.list) ?

   val bool   = (A.bool,   B.bool)
   val char   = (A.char,   B.char)
   val int    = (A.int,    B.int)
   val real   = (A.real,   B.real)
   val string = (A.string, B.string)
   val unit   = (A.unit,   B.unit)
   val word   = (A.word,   B.word)

   val largeInt  = (A.largeInt,  B.largeInt)
   val largeReal = (A.largeReal, B.largeReal)
   val largeWord = (A.largeWord, B.largeWord)

   val word8  = (A.word8,  B.word8)
   val word16 = (A.word16, B.word16)
   val word32 = (A.word32, B.word32)
   val word64 = (A.word64, B.word64)
end
