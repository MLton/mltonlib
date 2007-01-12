(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Signature for a low-level imperative polymorphic mapping from words to
 * values intended for building hash table like containers.  The idea is
 * that the client makes sure that the distribution of words used as keys
 * is sufficiently random, while the word table takes care of stuff like
 * resizing the table.
 *)

signature WORD_TABLE = sig
   eqtype 'a t
   (** The type of word tables. *)

   structure Key : WORD
   (** Substructure specifying the word type used as keys. *)

   val new : 'a t Thunk.t
   (** Allocates a new word table. *)

   val == : 'a t BinPr.t
   (** Equality predicate. *)

   val size : 'a t -> Int.t
   (** Returns the number of associations stored in the word table. *)

   (**
    * The {Action} substructure specifies type-safe combinators for
    * expressing actions to take on access.  In particular, the
    * combinators prevent the user from inserting or removing an element
    * multiple times during a single access.
    *)
   structure Action : sig
      type ('v, 'r) t
      type ('v, 'r, 's) m
      type some
      type none

      val get : {some : 'v -> ('v, 'r, some) m,
                 none : ('v, 'r, none) m Thunk.t} -> ('v, 'r) t
      val peek : {some : ('v, 'r, some) m Thunk.t,
                  none : ('v, 'r, none) m Thunk.t} -> ('v, 'r) t
      val insert : 'v -> 'r -> ('v, 'r, none) m
      val update : 'v -> 'r -> ('v, 'r, some) m
      val remove : 'r -> ('v, 'r, some) m
      val return : 'r -> ('v, 'r, 's) m
   end

   val access : 'v t -> Key.t -> ('v, 'r) Action.t -> 'r
   (** Performs an action on an association of the word table. *)
end
