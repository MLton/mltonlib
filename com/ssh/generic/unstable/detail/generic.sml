(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generic :> EXT_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open Basic Fn
   (* SML/NJ workaround --> *)

   structure Index = struct
      type ('a, 'x) t = 'x
      val getT = id
      val mapT = id

      type ('a, 'x) s = 'x
      val getS = id
      val mapS = id

      type ('a, 'k, 'x) p = 'x
      val getP = id
      val mapP = id
   end

   val nullary = id
   fun unary x x2y = x2y x
   fun binary xy xy2z = xy2z xy
   fun morph y _ y2x = y2x y

   val iso = morph
   val isoProduct = morph
   val isoSum = morph
   val op *` = binary
   val T = unary
   fun R _ = unary
   val tuple = unary
   val record = unary
   val op +` = binary
   fun C0 _ = nullary
   fun C1 _ = unary
   val data = unary
   val unit = nullary
   val Y = nullary
   val op --> = binary
   val exn = nullary
   fun regExn x _ x2ef = x2ef x
   val array = unary
   val refc = unary
   val vector = unary
   val largeInt = nullary
   val largeReal = nullary
   val largeWord = nullary
   val word8 = nullary
(* val word16 = nullary (* Word16 not provided by SML/NJ *) *)
   val word32 = nullary
   val word64 = nullary
   val list = unary
   val bool = nullary
   val char = nullary
   val int = nullary
   val real = nullary
   val string = nullary
   val word = nullary
end
