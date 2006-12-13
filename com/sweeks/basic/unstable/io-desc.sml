(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure IoDesc: IO_DESC = struct

   open OS.IO

   structure Kind = struct
      open OS.IO.Kind

      type t = iodesc_kind
         
      val == = op =
   end
      
   type t = iodesc

   val compare = Order.ofBasis o compare

end
