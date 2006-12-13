(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature ORDERED = sig

   type t

   val < : t * t -> Bool.t
   (**
    * x < y = toInt x < toInt y
    *)
   val <= : t * t -> Bool.t
   (**
    * x <= y = toInt x <= toInt y
    *)
   val > : t * t -> Bool.t
   (**
    * x > y = toInt x > toInt y
    *)
   val >= : t * t -> Bool.t
   (**
    * x >= y = toInt x >= toInt y
    *)
   val == : t * t -> Bool.t
   (**
    * == (x, y) is true iff x equals y.
    *)
   val compare: t * t -> Order.t
end
