(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Client} module for programming RPC clients.
 *)
signature CLIENT = sig
   exception Unknown
   (**
    * Raised when an attempt is made to call a declared procedure that is
    * not defined on the server.
    *)

   exception ProtocolMismatch
   (**
    * Raised during the connection process if the server doesn't support
    * the protocol of the client.
    *)

   structure Conn : sig
      type t
      (** The type of server connections. *)

      val close : t Effect.t
      (** Explicitly closes the connection. *)

      val byName : {host : String.t, port : Int.t} -> t
      (** Connects to the server on the specified host and port. *)

    (*val spawn : {exe : String.t, port : Int.t} -> t*)
   end

   structure Reply : sig
      type 'a t
      (** Type of asynchronous replies. *)

      val sync : 'a t -> 'a
      (** Waits for the asynchronous reply and returns it. *)
   end

   val declare : 'd Rep.t * 'c Rep.t * String.t -> Conn.t -> 'd -> 'c Reply.t
   (**
    * Declares a procedure with the given signature {(dom, cod, name)} and
    * allows it to be called through the given connection.
    *)
end
