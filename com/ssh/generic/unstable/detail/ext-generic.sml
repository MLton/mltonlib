(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure ExtGeneric :> EXT_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open Fn
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

   val iso = id
   val isoProduct = id
   val isoSum = id
   val op *` = id
   val T = id
   val R = id
   val tuple = id
   val record = id
   val op +` = id
   val C0 = id
   val C1 = id
   val data = id
   val unit = id
   val Y = id
   val op --> = id
   val exn = id
   val regExn = id
   val array = id
   val refc = id
   val vector = id
   val largeInt = id
   val largeReal = id
   val largeWord = id
   val word8 = id
(* val word16 = id (* Word16 not provided by SML/NJ *) *)
   val word32 = id
   val word64 = id
   val list = id
   val bool = id
   val char = id
   val int = id
   val real = id
   val string = id
   val word = id
end
