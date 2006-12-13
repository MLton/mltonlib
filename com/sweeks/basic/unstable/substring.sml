(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Substring: SUBSTRING = struct

   open VectorSlice

   type 'a elem = Char.t
   type t = Char.t t
   type 'a t0 = t
   type 'a base = String.t
   
end
