(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Signature for the {Emb} structure for embeddings. *)
signature EMB = sig
   type ('a, 'b) t = ('a -> 'b) * ('b -> 'a Option.t)
   (** Embedding of {'a} into {'b} with injection and projection functions. *)

   val id : ('a, 'a) t
   (**
    * The identity embedding.  This is always equivalent to {(fn a => a,
    * SOME)}.
    *)

   val to : ('a, 'b) t -> 'a -> 'b
   (** Extracts the injection part of the given embedding. *)

   val from : ('a, 'b) t -> 'b -> 'a Option.t
   (** Extracts the projection part of the given embedding. *)

   val <--> : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
   (** Embedding composition. *)
end
