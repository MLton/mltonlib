(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Imperative singly linked list node.  This is useful and often more
 * convenient than a functional list when implementing imperative data
 * structures.
 *
 * Note that imperative lists may form cycles and, unless otherwise
 * specified, procedures specified in this module are not specifically
 * designed to work with cyclic lists.
 *)

structure Node :> sig
   eqtype 'a t

   val new : 'a t Thunk.t
   (** Allocates a new empty node. *)

   val get : 'a t -> ('a * 'a t) Option.t
   (** Returns the contents of the node. *)

   val <- : ('a t * ('a * 'a t) Option.t) Effect.t
   (** Sets the contents of the node. *)

   val isEmpty : 'a t UnPr.t
   (** Returns true iff the imperative list is empty. *)

   val hd : 'a t -> 'a
   (**
    * Returns the first element of the imperative list.  Raises {Empty} if
    * the list is empty.
    *)

   val tl : 'a t -> 'a t
   (**
    * Returns the next node of the imperative list.  Raises {Empty} if the
    * list is empty.
    *)

   val push : 'a t -> 'a Effect.t
   (**
    * Inserts the given element into the imperative list after the given
    * node.
    *)

   val take : 'a t -> 'a Option.t
   (**
    * If the imperative list is non-empty, removes the first element {v}
    * of the list and returns {SOME v}.  Otherwise returns {NONE}.
    *)

   val drop : 'a t Effect.t
   (**
    * If the imperative list is non-empty, removes the first element of
    * the list.  Otherwise does nothing.
    *)

   val clearWith : 'a Effect.t -> 'a t Effect.t
   (**
    * Takes all elements of the imperative list of nodes one-by-one and
    * performs the given effect on the removed elements.
    *)

   val fromList : 'a List.t -> 'a t
   (** Constructs an imperative list from a functional list. *)

   val toList : 'a t -> 'a List.t
   (**
    * Returns a functional list containing the same elements as the imperative
    * list.
    *)

   val app : 'a Effect.t -> 'a t Effect.t
   (**
    * Applies the given effect to all elements of the imperative list.
    * {app} is to be implemented tail recursively.
    *)

   val find : 'a UnPr.t -> 'a t -> ('a t, 'a t) Sum.t
   (**
    * Returns {INR n} where {n} is first node containing an element
    * satisfying the given predicate or {INL n} where {n} is the last node
    * in the imperative list.  {find} is to be implemented tail
    * recursively.
    *)

   val length : 'a t -> Int.t
   (** Returns the length of the given imperative list. *)

   val filter : 'a UnPr.t -> 'a t UnOp.t
   (**
    * Drops all nodes from the imperative list whose elements do not
    * satisfy the given predicate.  Returns the last, and always empty,
    * node of the remaining list.
    *)

   val filterOut : 'a UnPr.t -> 'a t UnOp.t
   (**
    * Drops all nodes from the imperative list whose elements satisfy the
    * given predicate.  Returns the last, and always empty, node of the
    * remaining list.
    *)

   val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   (**
    * Folds the imperative lists with the given function and starting
    * value.  {foldl} is to be implemented tail recursively.
    *)
end = struct
   datatype 'a t = IN of ('a * 'a t) Option.t Ref.t
   fun new () = IN (ref NONE)
   fun get (IN t) = !t
   fun (IN r) <- t = r := t

   (* The following only use the operations {new}, {get}, and {<-}. *)

   fun fromList l = let
      val h = new ()
      fun lp ([], _) = ()
        | lp (x::xs, t) = let
         val t' = new ()
      in
         t <- SOME (x, t')
       ; lp (xs, t')
      end
   in
      lp (l, h)
    ; h
   end

   fun isEmpty t =
       not (isSome (get t))

   local
      fun eat t =
          case get t of
             NONE => raise Empty
           | SOME x => x
   in
      fun hd t = #1 (eat t)
      fun tl t = #2 (eat t)
   end

   fun push t x = let
      val n = new ()
   in
      n <- get t
    ; t <- SOME (x, n)
   end

   fun take t =
       case get t of
          NONE => NONE
        | SOME (x, t') => (t <- get t' ; SOME x)

   fun drop t =
       ignore (take t)

   fun clearWith e t =
       case take t of
          NONE => ()
        | SOME x => (e x : unit ; clearWith e t)

   fun foldl f x t =
       case get t of
          NONE => x
        | SOME (y, t) =>
          foldl f (f (y, x)) t

   fun toList n =
       rev (foldl op :: [] n)

   fun app e =
       foldl (e o #1) ()

   fun find p t =
       case get t of
          NONE => INL t
        | SOME (x, t') =>
          if p x then INR t else find p t'

   fun length n = foldl (1 <\ op + o #2) 0 n

   fun filter p t =
       case get t of
          NONE => t
        | SOME (x, t') => (if p x then () else drop t ; filter p t')

   fun filterOut p = filter (negate p)
end
