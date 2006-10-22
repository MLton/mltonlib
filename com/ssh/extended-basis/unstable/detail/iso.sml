(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Structure for isomorphisms.
 *)
structure Iso :> ISO = struct
   type ('a, 'b) iso = ('a, 'b) iso

   val id = (fn a => a, fn a => a)

   fun to (a2b, _) = a2b
   fun from (_, b2a) = b2a
end
