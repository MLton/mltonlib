(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure String = struct

   open String
      
   fun toSeq s =
      Seq.unfold (0, fn i =>
                  if i = size s then
                     None
                  else
                     Some (String.sub (s, i), i + 1))

end
