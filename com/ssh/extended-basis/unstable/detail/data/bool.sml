(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Bool : BOOL = struct
   structure Core = struct
      open Bool
      type bitwise = t
      type bounded = t
      type ordered = t
      type scannable = t
      type stringable = t
      val embString = (toString, fromString)
      val bounds = (false, true)
      val compare = fn (false, true) => LESS
                     | (true, false) => GREATER
                     | (_,        _) => EQUAL
      fun andb (b1, b2) = b1 andalso b2
      val notb = not
      fun orb (b1, b2) = b1 orelse b2
      val xorb = op <>
      val toInt = fn true => 1 | false => 0
   end

   structure Bounded = MkBounded (Core)
   structure Ordered = MkOrdered (Core)
   structure Scannable = MkScannable (Core)
   structure Stringable = MkStringable (Core)

   open Bounded
   open Ordered
   open Scannable
   open Stringable

   open Core
end
