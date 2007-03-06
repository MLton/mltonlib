(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Bool : BOOL = struct
   structure Core = struct
      open Bool
      type bounded = t
      type ordered = t
      type scannable = t
      type stringable = t
      val embString = (toString, fromString)
      val bounds = (false, true)
      val compare = fn (false, true) => LESS
                     | (true, false) => GREATER
                     | (_,        _) => EQUAL
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
