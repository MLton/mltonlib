(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Lazy: LAZY = struct

   fun memo th = let
      val r = ref None
   in
      fn () =>
      case !r of
         None => let
            val a = th ()
            val () = r := Some a
         in
            a
         end
      | Some a => a
   end

end

val lazy = Lazy.memo
