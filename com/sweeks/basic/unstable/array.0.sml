(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Array = struct

   type 'a t = 'a Array.t

   local
      open Array
   in
      val make = array
      val size = length
      val sub = sub
      val update = update
   end

   structure Unsafe = struct
      fun make n =
         if MLton.safe andalso n < 0 then
            raise Size
         else
            Primitive.Array.array n
      val sub = Unsafe.Array.sub
      val update = Unsafe.Array.update
   end

end
