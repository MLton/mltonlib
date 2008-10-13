(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature CLIENT = sig
   exception Unknown
   exception ProtocolMismatch

   structure Conn : sig
      type t
      val close : t Effect.t
      val byName : {host : String.t, port : Int.t} -> t
    (*val spawn : {exe : String.t, port : Int.t} -> t*)
   end

   structure Reply : sig
      type 'a t
      val sync : 'a t -> 'a
   end

   val declare : 'd Rep.t * 'c Rep.t * String.t -> Conn.t -> 'd -> 'c Reply.t
end
