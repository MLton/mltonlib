(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Signature for the {Emb} structure for embeddings. *)
signature EMB = sig
   type ('a, 'b) t = ('a -> 'b) * ('b -> 'a Option.t)
   (** Witness to an embedding of {'a} into {'b}. *)

   val id : ('a, 'a) t
   (** The identity embedding.  This is always equivalent to {(id, SOME)}. *)

   val to : ('a, 'b) t -> 'a -> 'b
   (** Extracts the injection part of the given embedding. *)

   val from : ('a, 'b) t -> 'b -> 'a Option.t
   (** Extracts the projection part of the given embedding. *)

   val <--> : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
   (** Embedding composition. *)
end
