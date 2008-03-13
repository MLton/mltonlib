(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Imperative dynamically growing buffer.  A (plain) buffer only allows
 * elements to be pushed to the end.  This simplifies the implementation.
 *
 * See also: {RESIZABLE_ARRAY}
 *)
signature BUFFER = sig
   type 'a t
   (** The type of buffers. *)

   (** == Constructors == *)

   val new : 'a t Thunk.t
   (** Creates a new empty buffer. *)

   val duplicate : 'a t UnOp.t
   (**
    * Creates a new duplicate of the buffer.  {duplicate b} is equivalent
    * to {let val b' = new () in pushBuffer b' b end}.
    *)

   (** == Capacity == *)

   val capacity : 'a t -> Int.t
   (**
    * Returns the maximum length after which it becomes necessary for the
    * buffer to allocate more storage for holding additional elements.  It
    * always holds that {length b <= capacity b}.
    *)

   val reserve : 'a t -> Int.t Effect.t
   (**
    * {reserve b n} attempts to ensure that {n <= capacity b}.  Does
    * nothing if the specified capacity is smaller than the current
    * capacity.  Also, the capacity of some type of buffers can not be
    * increased when they are empty.
    *
    * This can be used to avoid incremental (re)allocation when one knows
    * how many elements will be pushed into the buffer.
    *)

   val trim : 'a t Effect.t
   (**
    * Attempts to eliminate excess capacity allocated for the buffer.  In
    * other words, after {trim b} it should be that {capacity b - length
    * b} is as small as possible.
    *
    * Warning: Trim should be used with care as it can destroy asymptotic
    * complexity guarantees.
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

   (** == Iterators == *)

   val findSome : ('a -> 'b Option.t) -> 'a t -> 'b Option.t
   (**
    * Iterates over the buffer from the beginning applying the given
    * function.  Returns the first {SOME b} value returned by the function
    * or {NONE} if no such value is returned.
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

   val toString : Char.t t -> String.t
   (**
    * Returns the contents of a char buffer as a string.  {toString b} is
    * equivalent to {String.implode (toList b)}.
    *)

   val toCharArray : Char.t t -> CharArray.t
   (** Returns the contents of a char buffer as a {CharArray}. *)

   val toWord8Array : Word8.t t -> Word8Array.t
   (** Returns the contents of a {Word8} buffer as a {Word8Array}. *)

   val toWord8Vector : Word8.t t -> Word8Vector.t
   (** Returns the contents of a {Word8} buffer as a {Word8Vector}. *)

   val toVector : 'a t -> 'a Vector.t
   (**
    * Returns the contents of the buffer as a vector.  {toVector b} is
    * equivalent to {Vector.fromList (toList b)}.
    *)

   (** == Adding Elements to a Buffer ==
    *
    * It is generally guaranteed that adding elements to a buffer does not
    * reduce the capacity of the buffer.
    *)

   val push : 'a t -> 'a Effect.t
   (**
    * Adds an element to the tail of the buffer.  More precisely, after
    *
    *> val cb = toList b
    *> val () = push b v
    *> val ca = toList b
    *
    * it holds that {cb = init ca} and {last ca = v}.
    *
    * Assuming that {trim} is never called, then the amortized complexity
    * of {push} is O(1).
    *)

   val pushArray : 'a t -> 'a Array.t Effect.t
   (**
    * Adds the elements of the array to the buffer.  {pushArray b a} is
    * equivalent to {Array.app (push b) a}.
    *)

   val pushArraySlice : 'a t -> 'a ArraySlice.t Effect.t
   (**
    * Adds the elements of the slice to the buffer.  {pushArraySlice b s}
    * is equivalent to {ArraySlice.app (push b) s}.
    *)

   val pushBuffer : 'a t -> 'a t Effect.t
   (**
    * Adds the elements of the buffer to the buffer.  {pushBuffer b b'} is
    * equivalent to {pushList b (toList b')}.
    *)

   val pushList : 'a t -> 'a List.t Effect.t
   (**
    * Adds the elements of the list to the buffer.  {pushList b l} is
    * equivalent to {List.app (push b) l}.
    *)

   val pushVector : 'a t -> 'a Vector.t Effect.t
   (**
    * Adds the elements of the vector to the buffer.  {pushVector b v} is
    * equivalent to {Vector.app (push b) v}.
    *)

   val pushVectorSlice : 'a t -> 'a VectorSlice.t Effect.t
   (**
    * Adds the elements of the slice to the buffer.  {pushVectorSlice b
    * s} is equivalent to {VectorSlice.app (push b) s}.
    *)
end
