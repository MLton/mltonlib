(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Utility module for dealing with embeddings.
 *)

signature EMB =
   sig
      type ('a, 'b) emb = ('a -> 'b) * ('b -> 'a option)

      val id : ('a, 'a) emb

      val to : ('a, 'b) emb -> 'a -> 'b
      val from : ('a, 'b) emb -> 'b -> 'a option
   end

structure Emb :> EMB =
   struct
      type ('a, 'b) emb = ('a -> 'b) * ('b -> 'a option)

      val id = (fn a => a, SOME)

      fun to (a2b, _) = a2b
      fun from (_, b2a) = b2a
   end

type ('a, 'b) emb = ('a, 'b) Emb.emb
