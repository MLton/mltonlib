(** A generic deck of items. *)
signature DECK = sig
   structure RNG : RNG
   type 'a t
   exception Empty

   val fromList : 'a list * RNG.t -> 'a t
   val fromVector : 'a vector * RNG.t -> 'a t

   val toVector : 'a t -> 'a vector
   val toList : 'a t -> 'a list
					 
   val shuffle : 'a t -> unit
   (**
    * Shuffles the deck.  Removed items, if any, are put back in the
    * deck first.
    *)

   val take : 'a t -> 'a
   (**
    * Removes an item from the top of the deck.  If the deck is empty
    * the {Empty} exception will be thrown.
    *)

end
