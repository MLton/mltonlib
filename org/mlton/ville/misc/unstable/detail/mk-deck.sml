functor MkDeck (RNG : RNG) :> DECK where type RNG.t = RNG.t = struct
   structure RNG = RNG
   structure Random = MkRandom (RNG)
   datatype 'a t = D of {array : 'a array,
			 rng  : RNG.t,
			 taken : int ref}
   exception Empty

   fun new (array, rng) = D {array = array, rng = rng, taken = ref 0}
   fun fromVector (v, rng) = new (Array.fromVector v, rng)
   fun fromList (l, rng) = new (Array.fromList l, rng)
   fun size (D {array, ...}) = Array.length array

   (* XXX - move to basis extension? *)
   fun swap (arr, i, j) = let
      val a = Array.sub (arr, i)
      val b = Array.sub (arr, j)
   in
      Array.update (arr, j, a)
    ; Array.update (arr, i, b)
   end

   fun shuffle (D {taken, ...}) = taken := 0

   (* This is basically the Knuth shuffle (a.k.a. Fisher-Yates shuffle),
    * see http://en.wikipedia.org/wiki/Knuth_shuffle for more details.
    * We just don't shuffle the entire deck at once but gradually as
    * items are taken from the deck. *)
   fun take (D {array, rng, taken}) = let
      val i = !taken
      val numItems = Array.length array
      fun rand () = Random.generate rng (Random.intInRange (0, numItems - i))
   in
      if i >= numItems then
	 raise Empty
      else
	 (swap (array, i, i + rand ())
	; taken := i + 1
	; Array.sub (array, i))
   end

   fun toVector deck =
       (shuffle deck
      ; Vector.tabulate (size deck, fn _ => take deck))

   fun toList deck =
       (shuffle deck
      ; List.tabulate (size deck, fn _ => take deck))

end
