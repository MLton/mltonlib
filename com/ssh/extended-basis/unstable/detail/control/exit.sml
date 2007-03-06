(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Exit :> EXIT = struct
   type 'a t = 'a -> exn

   fun within block = let
      exception EscapedExit of 'a
   in
      block EscapedExit
      handle EscapedExit value => value
   end

   fun to exit value = raise exit value

   fun call block = within (block o to)
end
