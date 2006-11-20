(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Univ :> UNIV = struct
   type t = exn

   fun newIso () = let
      exception U of 'a
   in
      (U, fn U ? => ? | _ => raise Match)
   end

   fun newEmb () = let
      exception U of 'a
   in
      (U, fn U ? => SOME ? | _ => NONE)
   end
end
