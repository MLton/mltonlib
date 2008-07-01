(* Copyright (C) 2007-2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Cvt :> CVT = struct
   open BasisStringCvt

   type 'a t = 'a -> String.t

   type ('c, 's) sel = ('c -> 's) -> 's

   val fixSign = BasisString.map (fn #"~" => #"-" | other => other)

   val C = str
   val B = Bool.toString
   val D = Int.toString
   val D' = fixSign o Int.toString
   val X = Word.toString
   val G = Real.toString
   val G' = fixSign o Real.toString

   fun mk fmt k = k {b = fmt BIN,
                     o = fmt OCT,
                     d = fmt DEC,
                     x = fmt HEX}

   fun I k = mk Int.fmt k
   fun I' k = mk (fn c => fixSign o Int.fmt c) k

   fun W k = k {b = Word.fmt BIN,
                o = Word.fmt OCT,
                d = Word.fmt DEC,
                x = Word.fmt HEX}

   fun mk fmt k = k {s = fmt (SCI NONE),
                     S = fmt o SCI o SOME,
                     f = fmt (FIX NONE),
                     F = fmt o FIX o SOME,
                     g = fmt (GEN NONE),
                     G = fmt o GEN o SOME,
                     e = fmt EXACT}

   fun R k = mk Real.fmt k
   fun R' k = mk (fn c => fixSign o Real.fmt c) k

   fun seq prefix suffix foldr full get c xs =
       case get (full xs)
        of NONE => prefix ^ suffix
         | SOME (x, xs) =>
           concat (prefix::c x::foldr (fn (x, ss) => ", "::c x::ss) [suffix] xs)

   fun A ? = let open  ArraySlice in seq "#(" ")" foldr full  getItem end ?
   fun L ? = let open   List      in seq "["  "]" foldr Fn.id getItem end ?
   fun V ? = let open VectorSlice in seq "#[" "]" foldr full  getItem end ?

   fun O c = fn NONE => "NONE" | SOME x => "SOME " ^ c x

   fun P k = k {l = padLeft  #" ",
                r = padRight #" ",
                L = padLeft,
                R = padRight}
end
