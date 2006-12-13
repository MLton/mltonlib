(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure IntInf: INT_INF = struct

   open IntInf

   local
      structure S = Int (IntInf)
   in
      open S
   end

end
