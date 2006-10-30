(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Util =
   struct
      fun escapeJavascript (ws: Word.t vector) =
         let
            val w2c = Char.fromInt o Word.toInt
            val w2s = Char.toString o w2c
            val c2w = Word.fromInt o Char.toInt
            fun num w =
               Vector.fold (ws, 0, fn (w', n) => if w = w' then n + 1 else n)
            val sq = c2w #"'"
            val dq = c2w #"\""
            val quote = if Int.>= (num sq, num dq) then dq else sq
            val quoteS = w2s quote
            fun toHexDigits (w, n) =
               let
                  fun loop (n, w, ac) =
                     if n = 0
                        then implode ac
                     else loop (n - 1,
                                Word.div (w, 0wx10),
                                Char.fromHexDigit (Word.toInt
                                                   (Word.mod (w, 0wx10)))
                                :: ac)
               in
                  loop (n, w, [])
               end
            val body =
               concat
               (Vector.toListMap
                (ws, fn w =>
                 if w = quote then concat ["\\", w2s w]
                 else if w > 0wxFFFF then concat ["\\U", toHexDigits (w, 8)]
                 else if w > 0wxFF then concat ["\\u", toHexDigits (w, 4)]
                 else
                    case w2c w of
                       #"\000" => "\\0"
                     | #"\b" => "\\b"
                     | #"\f" => "\\f"
                     | #"\n" => "\\n"
                     | #"\r" => "\\r"
                     | #"\t" => "\\t"
                     | #"\v" => "\\v"
                     | #"\\" => "\\\\"
                     | c => if Char.isPrint c
                               then Char.toString c
                            else concat ["\\x", toHexDigits (w, 2)]))
         in
            concat [quoteS, body, quoteS]
         end

      fun realToJavascript r =
         if Real.equals (r, Real.realFloor r) then
            let
               val i = Real.toIntInf r
               val (isNeg, i) = if i < 0 then (true, ~i) else (false, i)
               val sD = IntInf.format (i, StringCvt.DEC)
               val sH = IntInf.format (i, StringCvt.HEX)
               val s =
                  if String.size sD <= String.size sH + 2 then
                     sD
                  else
                     concat ["0x", sH]
            in
               if isNeg then concat ["-", s] else s
            end
         else
            (* Want to lay out numbers nicely, but without losing precision.
             * Seventeen digits is enough to do this.
             *)
            String.translate
            (Real.format (r, Real.Format.gen (SOME 17)),
             fn #"~" => "-"
              | c => Char.toString c)
   end      
