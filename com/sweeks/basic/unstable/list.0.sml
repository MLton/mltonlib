(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure List = struct

   datatype t = datatype List.t
      
   fun fold (l, b, f) = let
      fun loop (l, b) =
         case l of
            [] => b
          | x :: l => loop (l, f (x, b))
   in
      loop (l, b)
   end

   fun reverse l = fold (l, [], op ::)

   fun toSeq l = Seq.unfold (l, fn [] => None | x :: l => Some (x, l))
      
end
