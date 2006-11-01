(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
end
