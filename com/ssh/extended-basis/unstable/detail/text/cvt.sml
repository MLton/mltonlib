(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Cvt :> CVT = struct
   open BasisStringCvt

   type 'a t = 'a -> String.t

   type ('c, 's) sel = ('c -> 's) -> 's

   val C = str
   val B = Bool.toString
   val D =  Int.toString
   val X = Word.toString
   val G = Real.toString

   fun I k = k {b = Int.fmt BIN,
                o = Int.fmt OCT,
                d = Int.fmt DEC,
                x = Int.fmt HEX}

   fun W k = k {b = Word.fmt BIN,
                o = Word.fmt OCT,
                d = Word.fmt DEC,
                x = Word.fmt HEX}

   fun R k = k {s = Real.fmt (SCI NONE),
                S = Real.fmt o SCI o SOME,
                f = Real.fmt (FIX NONE),
                F = Real.fmt o FIX o SOME,
                g = Real.fmt (GEN NONE),
                G = Real.fmt o GEN o SOME,
                e = Real.fmt EXACT}

   fun seq prefix suffix foldr full get c xs =
       case get (full xs)
        of NONE => prefix ^ suffix
         | SOME (x, xs) =>
           concat (prefix::c x::foldr (fn (x, ss) => ", "::c x::ss) [suffix] xs)

   fun A ? = let open  ArraySlice in seq "[|" "|]" foldr full  getItem end ?
   fun L ? = let open   List      in seq "["   "]" foldr Fn.id getItem end ?
   fun V ? = let open VectorSlice in seq "#["  "]" foldr full  getItem end ?

   fun O c = fn NONE => "NONE" | SOME x => "SOME " ^ c x

   fun P k = k {l = padLeft  #" ",
                r = padRight #" ",
                L = padLeft,
                R = padRight}
end
