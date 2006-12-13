(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Order = struct

   datatype t = Equal | Greater | Less

   local
      datatype z = datatype Basis.Order.t
   in
      val ofBasis =
         fn EQUAL => Equal
          | GREATER => Greater
          | LESS => Less
   end
(*    val toBasis =
 *       fn Equal => EQUAL
 *        | Greater => GREATER
 *        | Less => LESS
 *)

end
