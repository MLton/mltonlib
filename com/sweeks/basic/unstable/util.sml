(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Util: UTIL = struct

   fun const c _ = c

   fun die s = raise Fail s

   fun id x = x

   fun (f o g) x = f (g x)

   fun pass x f = f x
      
   fun recur (x, f) = let fun loop x = f (x, loop) in loop x end

   fun fst (a, _) = a

   fun snd (_, b) = b

end

open Util

