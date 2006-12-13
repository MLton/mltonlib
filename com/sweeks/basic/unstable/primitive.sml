(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Primitive = struct

   structure Array = struct
      val array = _prim "Array_array": int -> 'a array;
      (* Don't mutate the array after you apply fromArray, because vectors
       * are supposed to be immutable and the optimizer depends on this.
       *)
      val toVector = _prim "Array_toVector": 'a array -> 'a vector;
   end

end
