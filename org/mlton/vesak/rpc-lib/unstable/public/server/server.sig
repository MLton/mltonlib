(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SERVER = sig
   val run : {port : Int.t,
              accept : INetSock.sock_addr UnPr.t} Effect.t
   val define : 'd Rep.t * 'c Rep.t * String.t -> ('d -> 'c) Effect.t
end
