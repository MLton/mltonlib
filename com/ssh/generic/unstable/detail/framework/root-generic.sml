(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RootGeneric :> CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Open = struct
      structure Rep = struct
         type ('a,     'x) t = 'x
         type ('a,     'x) s = 'x
         type ('a, 'k, 'x) p = 'x

         val getT = id
         val getS = id
         val getP = id

         val mapT = id
         val mapS = id
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
      val regExn0 = id
      val regExn1 = id
      val array = id
      val refc = id
      val vector = id
      val fixedInt = id
      val largeInt = id
      val largeReal = id
      val largeWord = id
      val word8 = id
      val word32 = id
(*
      val word64 = id
*)
      val list = id
      val bool = id
      val char = id
      val int = id
      val real = id
      val string = id
      val word = id

      val hole = id
   end
end
