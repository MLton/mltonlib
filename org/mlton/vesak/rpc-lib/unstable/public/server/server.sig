(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Server} module for programming RPC servers.
*)
signature SERVER = sig
   val define : 'd Rep.t * 'c Rep.t * String.t -> ('d -> 'c) Effect.t
   (**
    * Defines a procedure with the given signature {(dom, cod, name)} and
    * implementation as callable via RPC.
    *)

   val run : {port : Int.t, accept : INetSock.sock_addr UnPr.t} Effect.t
   (**
    * Starts the server process listening on the specified port for
    * clients.
    *)
end
