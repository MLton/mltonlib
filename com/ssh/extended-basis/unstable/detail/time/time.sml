(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Time = struct
   open Time
   local
      fun mk f =
          (fn v => fromSeconds (v * f), fn t => LargeInt.quot (toSeconds t, f))
   in
      val (fromMinutes, toMinutes) = mk (60 * 60)
      val (fromHours, toHours) = mk 60
   end
end
