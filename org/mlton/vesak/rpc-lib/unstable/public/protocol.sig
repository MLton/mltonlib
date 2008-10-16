(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Protocol} module that exposes some RPC protocol
 * details mostly for diagnostic purposes.
 *)
signature PROTOCOL = sig
   (**
    * Each procedure needs to be given a signature that consist of the
    * types of the domain and the codomain of the procedure and an
    * arbitrary name.
    *)
   structure Signature : sig
      type ('d, 'c) t = 'd Rep.t * 'c Rep.t * String.t
   end

   (**
    * Procedures are actually identified by their fingerprints, which are
    * hashes of the signatures of the procedures.  Multiple different
    * signatures may have the same fingerprint, which is called a
    * collision.  When two (or more) procedures whose "natural" signatures
    * have the same fingerprint need to be put on a single server, it
    * should always be possible to change the names in the procedure
    * signatures to avoid the collision.
    *)
   structure Fingerprint : sig
      eqtype t
      (** Type of procedure fingerprints. *)

      val toString : t -> String.t
      (** Converts the fingerprint to a string for diagnostic messages. *)

      val fromSignature : ('d, 'c) Signature.t -> t
      (** Computes the fingerprint from the given procedure signatures. *)
   end

   (**
    * The protocol version is an arbitrary constant that is verified to
    * match on both the server and the client sides when a connection is
    * established.
    *)
   structure Version : sig
      eqtype t
      (** Type of protocol versions. *)

      val toString : t -> String.t
      (** Converts the protocol version to a string for diagnostic messages. *)

      val current : t
      (**
       * The version of the protocol that the program (client, server or
       * both) is being compiled against.
       *)
   end
end
