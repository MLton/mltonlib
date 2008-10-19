(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Client} module for programming RPC clients.
 *)
signature CLIENT = sig
   exception UnknownProcedure of Protocol.Fingerprint.t
   (**
    * Raised when an attempt is made to call a declared procedure that is
    * not defined on the server.
    *)

   exception ProtocolMismatch of Protocol.Version.t
   (**
    * Raised during the connection process if the server doesn't support
    * the protocol of the client.
    *)

   structure Conn : sig
      type t
      (** The type of server connections. *)

      val close : t Effect.t
      (** Explicitly closes the connection. *)
   end

   structure TCP : sig
      type connect_args
      type 'a connect = ('a, connect_args) FRU.upd
      val connect :
          ((connect_args,
            {host : String.t connect
             (** default: {"127.0.0.1"} *)
           , port : Int.t connect
             (** default: {45678} *)
           , tcpNoDelay : Bool.t connect
             (** default: {false} *)
            },
            Conn.t) FRU.args,
           'k) CPS.t
      (** Connects to the server on the specified host and port. *)
   end

   structure Reply : sig
      type 'a t
      (** Type of asynchronous replies. *)

      val sync : 'a t -> 'a
      (** Waits for the asynchronous reply and returns it. *)
   end

   val declare : ('d, 'c) Protocol.Signature.t -> Conn.t -> 'd -> 'c Reply.t
   (**
    * Declares a procedure with the given signature and allows it to be
    * called through the given connection.
    *)
end
