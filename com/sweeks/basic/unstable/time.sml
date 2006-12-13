(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Time: TIME = struct

   open Time
      
   type t = time

   val == = op =

   val compare = Order.ofBasis o compare

   val zero = zeroTime

   val ofSeconds = fromSeconds
   val ofMilliseconds = fromMilliseconds
   val ofMicroseconds = fromMicroseconds
   val ofNanoseconds = fromNanoseconds

   fun format (t, {fractionalDigits = n}) = fmt n t

   val scanner = Scanner.ofBasis scan

   fun ofString s = Scanner.scanString (scanner, s)

end
