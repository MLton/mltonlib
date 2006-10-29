(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Signature for the {Emb} structure for embeddings.
 *)
signature EMB = sig
   type ('a, 'b) emb = ('a -> 'b) * ('b -> 'a option)
   (**
    * Embedding of {'a} into {'b} with injection and projection functions.
    *)

   val id : ('a, 'a) emb
   (**
    * The identity embedding.  This is always equivalent to {(fn a => a,
    * SOME)}.
    *)

   val to : ('a, 'b) emb -> 'a -> 'b
   (**
    * Extracts the injection part of the given embedding.
    *)

   val from : ('a, 'b) emb -> 'b -> 'a option
   (**
    * Extracts the projection part of the given embedding.
    *)
end
