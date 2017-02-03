(**
 * This is a signature for managing probability vectors (or stochastic
 * vectors) for representing discrete probability distributions.
 *
 * With this signature, probabilities do not need to be normalized.
 * In other words, the items in a vector do not have to add up to one.
 * If needed, a normalized vector can be computed with {normalize}.
 *)
signature PROBABILITY_VECTOR = sig
   type t
   (** The type of a probability vector. *)

   type item
   (** The type of items for which probabilities are being managed. *)

   val items : item vector
   (**
    * Contains all the items for which a probability is given by a
    * probability vector.
    *)

   val make : real -> t
   (**
    * {make p} creates a new probability vector for {items} where the
    * (non-normalized) probability of each item is {p}.
    *)

   val tabulate : (item -> real) -> t
   (**
    * {tabulate f} creates a new probability vector and initializes the
    * probabilities by calling {f} for each item.
    *)

   val map : t * (real * item -> real) -> t
   (**
    * {map (d, f)} calls {f (p, i)} for each item {i} and
    * probability {p}, and sets the new (non-normalized) probabilities
    * according to values returned by {f}.
    *)

   val app : t * (real * item -> unit) -> unit

   val toList : t -> (real * item) list
   val toVector : t -> real vector

   val fromArray : real array -> t

   val mul : t * t -> t
   (**
    * {mul (a, b)} multiplies each pair of probabilities in the two
    * vectors and produces a new probability vector of the results.
    *)

   val fold : t * (real * item * 'a -> 'a) * 'a -> 'a

   val normalize : t -> t
   (**
    * Returns a normalized version of the probability vector, where
    * the probabilities add up to {1.0} (approximately).
    *)

end
