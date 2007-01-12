(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A functor for combining implementations of the {STRUCTURAL_TYPE}
 * signature.
 *)

functor StructuralTypePair
           (structure A : STRUCTURAL_TYPE
            structure B : STRUCTURAL_TYPE) : STRUCTURAL_TYPE = struct
   type 'a t = 'a A.t * 'a B.t

   fun iso (a, b) i = (A.iso a i, B.iso b i)

   local
      fun mk t = Pair.map t o Pair.swizzle
   in
      fun op *`  ? = mk (A.*`,  B.*`)  ?
      fun op +`  ? = mk (A.+`,  B.+`)  ?
      fun op --> ? = mk (A.-->, B.-->) ?
   end

   fun Y ? = Tie.tuple2 (A.Y, B.Y) ?

   val exn = (A.exn, B.exn)
   fun regExn (a, b) emb = (A.regExn a emb ; B.regExn b emb)

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
