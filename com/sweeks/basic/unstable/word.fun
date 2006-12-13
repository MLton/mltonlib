(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
functor Word (structure Word: BASIS_WORD): WORD = struct

   open Word

   type t = word

   val == = op =

   val compare = Order.ofBasis o compare

   val ofLarge = fromLarge

   fun toStringRadix (w, r) = Word.fmt (Radix.toBasis r) w

   fun toString w = toStringRadix (w, Radix.hex)

   fun scanner r = Scanner.ofBasis (Word.scan (Radix.toBasis r))

   fun ofStringRadix (s, r) = Scanner.scanString (scanner r, s)

   fun ofString s = ofStringRadix (s, Radix.hex)
   
end
