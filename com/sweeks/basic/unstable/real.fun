(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
functor Real (structure Real: BASIS_REAL): REAL = struct

   open Real RealStructs

   open Math

   type t = real

   val compare = Order.ofBasis o compare

   fun format (r, f) = fmt f r

   val class = Class.ofBasis o class

   val ofDecimal = Option.ofBasis o fromDecimal o Decimal.toBasis

   val ofInt = fromInt

   fun ofLarge (r, m) = fromLarge m r

   val ofLargeInt = fromLargeInt

   val ofManExp = fromManExp

   val toDecimal = Decimal.ofBasis o toDecimal

   val scanner = Scanner.ofBasis Real.scan

   fun ofString s = Scanner.scanString (scanner, s)

   fun toInt (r, m) = Real.toInt m r

   fun toLargeInt (r, m) = Real.toLargeInt m r

end
