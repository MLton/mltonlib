(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor PairGenerics (structure F : GENERIC
                      structure S : GENERIC) : GENERIC = struct
   structure Index = struct
      type 'a t = 'a F.Index.t * 'a S.Index.t
      type 'a s = 'a F.Index.s * 'a S.Index.s
      type ('a, 'b) p = ('a, 'b) F.Index.p * ('a, 'b) S.Index.p
   end

   local
      fun mk aIso bIso (a, b) i = (aIso a i, bIso b i)
   in
      fun iso        ? = mk F.iso        S.iso        ?
      fun isoProduct ? = mk F.isoProduct S.isoProduct ?
      fun isoSum     ? = mk F.isoSum     S.isoSum     ?
   end

   local
      fun mk t = Pair.map t o Pair.swizzle
   in
      fun op *`  ? = mk (F.*`,  S.*`)  ?
      fun op +`  ? = mk (F.+`,  S.+`)  ?
      fun op --> ? = mk (F.-->, S.-->) ?
   end

   fun T ? = Pair.map (F.T, S.T) ?
   fun R ? = Pair.map (F.R ?, S.R ?)

   fun C0 ? = (F.C0 ?, S.C0 ?)
   fun C1 ? = Pair.map (F.C1 ?, S.C1 ?)

   fun Y ? = Tie.tuple2 (F.Y, S.Y) ?

   val exn = (F.exn, S.exn)
   fun regExn (a, b) emb = (F.regExn a emb ; S.regExn b emb)

   fun tuple  ? = Pair.map (F.tuple,  S.tuple)  ?
   fun record ? = Pair.map (F.record, S.record) ?
   fun data   ? = Pair.map (F.data,   S.data)   ?

   fun array ? = Pair.map (F.array, S.array) ?
   fun refc  ? = Pair.map (F.refc,  S.refc)  ?

   fun vector ? = Pair.map (F.vector, S.vector) ?

   fun list ? = Pair.map (F.list, S.list) ?

   val bool   = (F.bool,   S.bool)
   val char   = (F.char,   S.char)
   val int    = (F.int,    S.int)
   val real   = (F.real,   S.real)
   val string = (F.string, S.string)
   val unit   = (F.unit,   S.unit)
   val word   = (F.word,   S.word)

   val largeInt  = (F.largeInt,  S.largeInt)
   val largeReal = (F.largeReal, S.largeReal)
   val largeWord = (F.largeWord, S.largeWord)

   val word8  = (F.word8,  S.word8)
   val word16 = (F.word16, S.word16)
   val word32 = (F.word32, S.word32)
   val word64 = (F.word64, S.word64)
end
