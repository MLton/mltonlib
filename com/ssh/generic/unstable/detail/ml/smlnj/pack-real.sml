(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   functor MkPackReal (include CAST_REAL
                       val isBigEndian : bool) : PACK_REAL = struct
      val (toBits, fromBits) = valOf isoBits
      type real = t
      val bytesPerElem = Bits.wordSize div 8
      val isBigEndian = isBigEndian
      val shift = if isBigEndian
                  then fn i => Word.fromInt Bits.wordSize - 0w8 -
                               Word.<< (Word.fromInt i, 0w3)
                  else fn i => Word.<< (Word.fromInt i, 0w3)
      fun tabulator r = let
         val w = toBits r
      in
         fn i => Word8.fromInt (Bits.toIntX (Bits.andb (Bits.>> (w, shift i),
                                                        Bits.fromInt 0xFF)))
      end
      fun sub sub = let
         fun lp (w, i) =
             if i = bytesPerElem
             then fromBits w
             else lp (Bits.orb (w,
                                Bits.<< (Bits.fromInt (Word8.toInt (sub i)),
                                         shift i)),
                      i + 1)
      in
         lp (Bits.fromInt 0, 0)
      end
      fun toBytes r = Word8Vector.tabulate (bytesPerElem, tabulator r)
      fun fromBytes b = sub (fn i => Word8Vector.sub (b, i))
      fun subVec (v, i) =
          sub let val s = i*bytesPerElem in fn i => Word8Vector.sub (v, s+i) end
      fun subArr (a, i) =
          sub let val s = i*bytesPerElem in fn i => Word8Array.sub (a, s+i) end
      fun update (a, i, r) =
         Word8ArraySlice.modifyi
            (tabulator r o #1)
            (Word8ArraySlice.slice (a, i*bytesPerElem, SOME bytesPerElem))
   end
in
   structure PackReal64Big       : PACK_REAL where type real = Real64.real =
      MkPackReal (open CastReal val isBigEndian = true)
   structure PackReal64Little    : PACK_REAL where type real = Real64.real =
      MkPackReal (open CastReal val isBigEndian = false)
   structure PackRealBig         : PACK_REAL where type real = Real.real =
      PackReal64Big
   structure PackRealLittle      : PACK_REAL where type real = Real.real =
      PackReal64Little
   structure PackLargeRealBig    : PACK_REAL where type real = LargeReal.real =
      PackReal64Big
   structure PackLargeRealLittle : PACK_REAL where type real = LargeReal.real =
      PackReal64Little
end
