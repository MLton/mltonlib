(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Server} module for programming RPC servers.
 *)
signature SERVER = sig

   structure ProcMap : sig
      type t
      (** Type of procedure maps. *)

      val new : t Thunk.t
      (** Creates a new procedure map. *)

      val add : t -> ('d, 'c) Protocol.Signature.t -> ('d -> 'c) Effect.t
      (**
       * Adds a procedure with the given signature and implementation to
       * the procedure map.
       *)
   end

   structure TCP : sig
      type start_args
      type 'a start = ('a, start_args) FRU.upd
      val start :
          ProcMap.t ->
          ((start_args,
            {name : String.t start
             (** default: {"127.0.0.1"} *)
           , port : Int.t start
             (** default: {45678} *)
           , numAccepts : Int.t Option.t start
             (** default: {45678} *)
           , tcpNoDelay : Bool.t start
             (** default: {false} *)
           , serverError : Exn.t Effect.t start
             (** default: {ignore} *)
           , closed : Unit.t Effect.t start
             (** default: {ignore} *)
           , accept : {addr : INetSock.sock_addr} UnPr.t start
             (** default: {const true} *)
           , protocolMismatch :
             {addr : INetSock.sock_addr,
              version : Protocol.Version.t} Effect.t start
             (** default: {ignore} *)
           , connected : {addr : INetSock.sock_addr} Effect.t start
             (** default: {ignore} *)
           , unknownProc :
             {addr : INetSock.sock_addr,
              fingerprint : Protocol.Fingerprint.t} Effect.t start
             (** default: {ignore} *)
           , protocolError :
             {addr : INetSock.sock_addr, error : Exn.t} Effect.t start
             (** default: {ignore} *)
           , disconnected : {addr : INetSock.sock_addr} Effect.t start
             (** default: {ignore} *)
            },
            Unit.t) FRU.args,
           'k) CPS.t
      (**
       * Starts an async server handler listening on the specified {name}d
       * address and {port} for clients using the TCP protocol.
       *)
   end

   val run : Unit.t Effect.t
   (**
    * Runs the started server(s).  Calling {run ()} is equivalent to
    * executing:
    *
    *> PollLoop.run Handler.runAll
    *)
end
