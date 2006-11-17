(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Imperative dynamically growing buffer. *)
signature BUFFER = sig
   type 'a t

   val new : 'a t Thunk.t

   val duplicate : 'a t UnOp.t

   val length : 'a t -> Int.t

   val sub : 'a t * Int.t -> 'a

   val push : ('a t * 'a) Effect.t

   val pushArray : ('a t * 'a Array.t) Effect.t
   val pushArraySlice : ('a t * 'a ArraySlice.t) Effect.t
   val pushVector : ('a t * 'a Vector.t) Effect.t
   val pushVectorSlice : ('a t * 'a VectorSlice.t) Effect.t

   val toArray : 'a t -> 'a Array.t
   val toVector : 'a t -> 'a Vector.t
end
