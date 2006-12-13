(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Word8 = struct
   open Word8
   type t = word
end

structure LargeWord = struct
   open LargeWord
   type t = word
end

signature WORD = sig

   include ORDERED

   val + : t * t -> t
   (**
    * i + j returns the sum of i and j.
    *)
   val - : t * t -> t
   (**
    * i - j returns the difference of i and j.
    *)
   val * : t * t -> t
   (**
    * w1 - w2 returns the difference of w1 and w2.
    *)
   val << : t * Word.t -> t
   (**
    * << (w1, w2) shifts w1 to the left by w2 bit positions.
    * Logical shift left.
    *)
   val >> : t * Word.t -> t
   (**
    * >> (w1, w2) shifts w1 to the right by w2 bit positions.
    * Logical shift right.
    *)
   val ~>> : t * Word.t -> t
   (**
    * >> (w1, w2) shifts w1 to the right by w2 bit positions.
    * Arithmetic shift right.
    *)
   val andb: t * t -> t
   (**
    * andb (w1, w2) returns the "bitwise and" of w1 and w2.
    *)
   val div: t * t -> t
   (**
    * i div j returns floor (i/j).  It may raise Overflow.
    * i div 0 raises Div.
    *)
   val mod: t * t -> t
   (**
    * i mod j returns the remainder of the division of i by j.
    * i mod j has the same sign as j.
    * i mod 0 raises Div.
    * i = j * (i div j) + i mod j.
    *)
   val notb: t -> t
   (**
    * notb w returns the "bitwise not" of w.
    *)
   val ofLarge: LargeWord.t -> t
   val ofString: String.t -> t Option.t
   (**
    * ofString s = ofStringRadix (s, Radix.hex)
    *)
   val ofStringRadix: String.t * Radix.t -> t Option.t
   (**
    * ofStringRadix (s, r) returns Some i if s is the representation of i in
    * radix r.
    *)
   val orb: t * t -> t
   (**
    * orb (w1, w2) returns the "bitwise or" of w1 and w2.
    *)
   val scanner: Radix.t -> t Scanner.t
   (**
    * scanner r returns a scanner for words where characters are interepreted
    * according to radix r.
    *)
   val toString: t -> String.t
   (**
    * toString i = toStringRadix (i, Radix.Dec)
    *)
   val toStringRadix: t * Radix.t -> String.t
   val toLarge: t -> LargeWord.t
   val toLargeX: t -> LargeWord.t
   (**
    * toStringRadix (i, r) returns the string representation of i in radix r.
    *)
   val xorb: t * t -> t
   (**
    * xorb (w1, w2) returns the "bitwise xor" of w1 and w2.
    *)
end
