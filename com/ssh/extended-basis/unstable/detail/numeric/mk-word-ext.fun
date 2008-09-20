(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkWordExt (W : BASIS_WORD) : WORD = struct
   structure Core = struct
      open W
      type t = word
      type bitwise = t
      type bounded = t
      type formattable = t
      type formattable_format = BasisStringCvt.radix
      type intable = t
      type largeable = t
      type largeable_large = BasisLargeWord.word
      type ordered = t
      type scannable = t
      type scannable_format = formattable_format
      type shiftable = t
      type stringable = t
      type wordable = t
      val largestPrime =
          (~ o fromInt)
             (case wordSize of
                   1 =>   1 |   2 =>   1 |   3 =>   1 |   4 =>   3
               |   5 =>   1 |   6 =>   3 |   7 =>   1 |   8 =>   5
               |   9 =>   3 |  10 =>   3 |  11 =>   9 |  12 =>   3
               |  13 =>   1 |  14 =>   3 |  15 =>  19 |  16 =>  15
               |  17 =>   1 |  18 =>   5 |  19 =>   1 |  20 =>   3
               |  21 =>   9 |  22 =>   3 |  23 =>  15 |  24 =>   3
               |  25 =>  39 |  26 =>   5 |  27 =>  39 |  28 =>  57
               |  29 =>   3 |  30 =>  35 |  31 =>   1 |  32 =>   5
               |  33 =>   9 |  34 =>  41 |  35 =>  31 |  36 =>   5
               |  37 =>  25 |  38 =>  45 |  39 =>   7 |  40 =>  87
               |  41 =>  21 |  42 =>  11 |  43 =>  57 |  44 =>  17
               |  45 =>  55 |  46 =>  21 |  47 => 115 |  48 =>  59
               |  49 =>  81 |  50 =>  27 |  51 => 129 |  52 =>  47
               |  53 => 111 |  54 =>  33 |  55 =>  55 |  56 =>   5
               |  57 =>  13 |  58 =>  27 |  59 =>  55 |  60 =>  93
               |  61 =>   1 |  62 =>  57 |  63 =>  25 |  64 =>  59
               |  65 =>  49 |  66 =>   5 |  67 =>  19 |  68 =>  23
               |  69 =>  19 |  70 =>  35 |  71 => 231 |  72 =>  93
               |  73 =>  69 |  74 =>  35 |  75 =>  97 |  76 =>  15
               |  77 =>  33 |  78 =>  11 |  79 =>  67 |  80 =>  65
               |  81 =>  51 |  82 =>  57 |  83 =>  55 |  84 =>  35
               |  85 =>  19 |  86 =>  35 |  87 =>  67 |  88 => 299
               |  89 =>   1 |  90 =>  33 |  91 =>  45 |  92 =>  83
               |  93 =>  25 |  94 =>   3 |  95 =>  15 |  96 =>  17
               |  97 => 141 |  98 =>  51 |  99 => 115 | 100 =>  15
               | 101 =>  69 | 102 =>  33 | 103 =>  97 | 104 =>  17
               | 105 =>  13 | 106 => 117 | 107 =>   1 | 108 =>  59
               | 109 =>  31 | 110 =>  21 | 111 =>  37 | 112 =>  75
               | 113 => 133 | 114 =>  11 | 115 =>  67 | 116 =>   3
               | 117 => 279 | 118 =>   5 | 119 =>  69 | 120 => 119
               | 121 =>  73 | 122 =>   3 | 123 =>  67 | 124 =>  59
               | 125 =>   9 | 126 => 137 | 127 =>   1 | 128 => 159
               | _  => raise Fail ("largestPrime less than pow (2, "^
                                   BasisInt.toString wordSize^") not known"))
      val bounds = (fromInt 0, fromInt~1)
      val numBytes = BasisInt.quot (BasisInt.+ (wordSize, 7), 8)
      val fromWord8 = fromInt o BasisWord8.toInt
      val fromWord8X = fromInt o BasisWord8.toIntX
      local
         fun mk fold bs =
             if numBytes <> BasisWord8Vector.length bs then
                raise Subscript
             else
                fold (fn (b, w) => W.orb (W.<< (w, 0w8), fromWord8 b))
                     (W.fromInt 0)
                     bs
      in
         val fromBigBytes = mk BasisWord8Vector.foldl
         val fromLittleBytes = mk BasisWord8Vector.foldr
      end
      fun intPrec p = case BasisInt.precision of NONE => false | SOME n => p n
      val toFixedInt =
          if intPrec (fn n => BasisInt.< (wordSize, n))
          then BasisFixedInt.fromInt o toInt
          else BasisFixedInt.fromLarge o toLargeInt
      val toFixedIntX =
          if intPrec (fn n => BasisInt.<= (wordSize, n))
          then BasisFixedInt.fromInt o toIntX
          else BasisFixedInt.fromLarge o toLargeIntX
      val maxAsWord =
          BasisWord.- (BasisWord.<< (0w1, BasisWord.fromInt wordSize), 0w1)
      val fromWord =
          if intPrec (fn n => n = BasisWord.wordSize andalso n = wordSize)
          then fromInt o BasisWord.toIntX
          else if intPrec (fn n => BasisInt.< (wordSize, n))
          then fromInt o
               (if intPrec (fn n => n = BasisWord.wordSize)
                then BasisWord.toIntX
                else BasisWord.toInt o (fn w => BasisWord.andb (maxAsWord, w)))
          else if BasisInt.<= (wordSize, BasisLargeWord.wordSize)
          then fromLarge o BasisWord.toLarge
          else fromLargeInt o BasisWord.toLargeInt (* SML/NJ workaround *)
      val fromWordX =
          if intPrec (fn n => n = BasisWord.wordSize andalso n = wordSize)
          then fromInt o BasisWord.toIntX
          else fromLarge o BasisWord.toLargeX
      fun mask m w = W.andb (w, fromInt m)
      val toWord8 =
          BasisWord8.fromInt o
          (if intPrec (fn n => n = wordSize)
           then toIntX
           else if intPrec (fn n => BasisInt.< (n, wordSize))
           then toIntX o mask 0xFF
           else toInt)
      val toWord8X = BasisWord8.fromLarge o toLargeX
      local
         fun mk idx w =
             BasisWord8Vector.tabulate
                (numBytes,
                 fn i =>
                    (toWord8 o W.>>)
                       (w, BasisWord.<< (BasisWord.fromInt (idx i), 0w3)))
      in
         val toBigBytes = mk (fn i => BasisInt.- (BasisInt.- (numBytes, 1), i))
         val toLittleBytes = mk (fn i => i)
      end
      val toWord =
          if intPrec (fn n => n = BasisWord.wordSize andalso n = wordSize)
          then BasisWord.fromInt o toIntX
          else if intPrec (fn n => BasisInt.< (wordSize, n))
          then BasisWord.fromInt o toInt
          else if BasisInt.<= (wordSize, BasisLargeWord.wordSize)
          then BasisWord.fromLarge o toLarge
          else BasisWord.fromLargeInt o toLargeInt (* SML/NJ workaround *)
      val toWordX =
          if intPrec (fn n => n = BasisWord.wordSize andalso n = wordSize)
          then BasisWord.fromInt o toIntX
          else BasisWord.fromLarge o toLargeX
      val fromFixedInt =
          if intPrec (fn n => n = valOf BasisFixedInt.precision)
          then fromInt o BasisFixedInt.toInt
          else fromLargeInt o BasisFixedInt.toLarge
      val embString = (toString, fromString)
      val isoBigBytes = (toBigBytes, fromBigBytes)
      val isoFixedInt = (toFixedInt, fromFixedInt)
      val isoFixedIntX = (toFixedIntX, fromFixedInt)
      val isoInt = (toInt, fromInt)
      val isoIntX = (toIntX, fromInt)
      val isoLarge = (toLarge, fromLarge)
      val isoLargeInt = (toLargeInt, fromLargeInt)
      val isoLargeIntX = (toLargeIntX, fromLargeInt)
      val isoLargeX = (toLargeX, fromLarge)
      val isoLittleBytes = (toLittleBytes, fromLittleBytes)
      val isoWord = (toWord, fromWord)
      val isoWord8 = (toWord8, fromWord8)
      val isoWord8X = (toWord8X, fromWord8X)
      val isoWordX = (toWordX, fromWordX)
      val isoLargeWord = isoLarge
      val isoLargeWordX = isoLargeX
      fun isZero w = fromInt 0 = w
      fun isEven w = isZero (andb (fromInt 1, w))
      val isOdd = not o isEven
      fun sq x = x * x
   end

   structure Bounded = MkBounded (Core)
   structure Ordered = MkOrdered (Core)
   structure Stringable = MkStringable (Core)

   open Bounded
   open Ordered
   open Stringable

   open Core
end
