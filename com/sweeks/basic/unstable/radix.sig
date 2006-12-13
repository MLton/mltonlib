(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature RADIX = sig

   type t
   (**
    * A radix used for integer<->string conversions.
    *)

   val bin: t
   val dec: t
   val hex: t
   val oct: t

   val toString: t -> String.t
   (**
    * toString r returns a human-readable string for r.
    *)

end
