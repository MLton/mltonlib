(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Imperative dynamically growing buffer. *)
signature BUFFER = sig
   type 'a t
   (** The type of buffers. *)

   (** == Constructors == *)

   val new : 'a t Thunk.t
   (** Creates a new empty buffer. *)

   val duplicate : 'a t UnOp.t
   (**
    * Creates a new duplicate of the buffer.  {duplicate b} is equivalent
    * to {let val b' = new () in pushBuffer (b', b) end}.
    *)

   (** == Accessors == *)

   val isEmpty : 'a t UnPr.t
   (** {isEmpty b} is equivalent to {0 = length b}. *)

   val length : 'a t -> Int.t
   (** Returns the number of elements pushed into the buffer. *)

   val sub : 'a t * Int.t -> 'a
   (**
    * {sub (b, i)} returns the {i}th element of the buffer {b}.  If {i <
    * 0} or {length b <= i}, then the {Subscript} exception is raised.
    *)

   (** == Conversions == *)

   val toArray : 'a t -> 'a Array.t
   (**
    * Creates a new array of the contents of the buffer.  {toArray b} is
    * equivalent to {Array.fromList (toList b)}.
    *)

   val toList : 'a t -> 'a List.t
   (**
    * Returns the contents of the buffer as a list.  {toList b} is
    * equivalent to {List.tabulate (length b, b <\ sub)}.
    *)

   val toVector : 'a t -> 'a Vector.t
   (**
    * Returns the contents of the buffer as a vector.  {toVector b} is
    * equivalent to {Vector.fromList (toList b)}.
    *)

   (** == Adding Elements to a Buffer == *)

   val push : ('a t * 'a) Effect.t
   (**
    * Adds an element to the tail of the buffer.  More precisely, after
    *
    *> val cb = toList b
    *> val () = push (b, v)
    *> val ca = toList b
    *
    * it holds that {cb = init ca} and {last ca = v}.
    *)

   val pushArray : ('a t * 'a Array.t) Effect.t
   (**
    * Adds the elements of the array to the buffer.  {pushArray (b, a)} is
    * equivalent to {Array.app (b <\ push) a}.
    *)

   val pushArraySlice : ('a t * 'a ArraySlice.t) Effect.t
   (**
    * Adds the elements of the slice to the buffer.  {pushArraySlice (b,
    * s)} is equivalent to {ArraySlice.app (b <\ push) s}.
    *)

   val pushBuffer : ('a t * 'a t) Effect.t
   (**
    * Adds the elements of the buffer to the buffer.  {pushVectorSlice (b,
    * b')} is equivalent to {pushList (b, toList b')}.
    *)

   val pushList : ('a t * 'a List.t) Effect.t
   (**
    * Adds the elements of the list to the buffer.  {pushList (b, l)} is
    * equivalent to {List.app (b <\ push) l}.
    *)

   val pushVector : ('a t * 'a Vector.t) Effect.t
   (**
    * Adds the elements of the vector to the buffer.  {pushVector (b, v)}
    * is equivalent to {Vector.app (b <\ push) v}.
    *)

   val pushVectorSlice : ('a t * 'a VectorSlice.t) Effect.t
   (**
    * Adds the elements of the slice to the buffer.  {pushVectorSlice (b,
    * s)} is equivalent to {VectorSlice.app (b <\ push) s}.
    *)
end
