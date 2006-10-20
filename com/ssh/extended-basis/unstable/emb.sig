(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type ('a, 'b) emb = ('a -> 'b) * ('b -> 'a option)
(**
 * Embedding of {'a} into {'b} with injection and projection functions.
 *)

(**
 * Signature for the {Emb} structure for embeddings.
 *)
signature EMB = sig
   type ('a, 'b) emb = ('a, 'b) emb

   val id : ('a, 'a) emb

   val to : ('a, 'b) emb -> 'a -> 'b
   val from : ('a, 'b) emb -> 'b -> 'a option
end
