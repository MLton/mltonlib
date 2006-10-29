(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Structure for embeddings.
 *)
structure Emb :> EMB = struct
   type ('a, 'b) emb = ('a -> 'b) * ('b -> 'a option)

   val id = (fn a => a, SOME)

   fun to (a2b, _) = a2b
   fun from (_, b2a) = b2a
end
