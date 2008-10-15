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
      structure Opts : sig
         type t and 'a opt

         val default : t
         (** Default options. *)

         (** == Updating Options ==
          *
          * Example:
          *
          *> default & port := 4321
          *>         & maxAccepts := SOME 1
          *)

         val & : t * ('a opt * 'a) -> t
         val := : ('a opt * 'a) UnOp.t

         (** == Server Settings == *)

         val name : String.t opt
         (** default: {"127.0.0.1"} *)

         val port : Int.t opt
         (** default: 45678 *)

         val maxAccepts : Int.t Option.t opt
         (** default: {NONE} *)

         val tcpNoDelay : Bool.t opt
         (** default: {true} *)

         (** == Server Events == *)

         val serverError : Exn.t Effect.t opt
         (** default: {ignore} *)

         val closed : Unit.t Effect.t opt
         (** default: {ignore} *)

         val accept : {addr : INetSock.sock_addr} UnPr.t opt
         (** default: {const true} *)

         val unknownProtocol :
             {addr : INetSock.sock_addr,
              version : Protocol.Version.t} Effect.t opt
         (** default: {ignore} *)

         val connected :
             {addr : INetSock.sock_addr,
              version : Protocol.Version.t} Effect.t opt
         (** default: {ignore} *)

         val unknownProc :
             {addr : INetSock.sock_addr,
              fingerprint : Protocol.Fingerprint.t} Effect.t opt
         (** default: {ignore} *)

         val protocolError :
             {addr : INetSock.sock_addr,
              error : Exn.t} Effect.t opt
         (** default: {ignore} *)

         val disconnected : {addr : INetSock.sock_addr} Effect.t opt
         (** default: {ignore} *)
      end

      val start : ProcMap.t -> Opts.t Effect.t
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
