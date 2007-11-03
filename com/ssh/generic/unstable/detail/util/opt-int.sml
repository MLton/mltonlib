(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure OptInt = struct
   type t = Int.t Option.t
   local
      fun mk bop =
       fn (SOME l, SOME r) => SOME (bop (l, r))
        | _                => NONE
   in
      val op +   = mk op +
      val op -   = mk op -
      val op div = mk op div
   end
end
