functor MkRandom (RNG : RNG) = struct
   structure RNG = RNG
   type 'a t = RNG.t -> 'a

   fun generate rng convert = convert rng

  (*
   * XXX (bug): This suffers from the modulo bias.  The effect
   * is small if small ranges are used, but if the range is
   * a significant fraction of the word width this is bad.
   *)
   fun wordInRange (l, h) rng = l + Word.mod (RNG.rand rng, h - l)

   fun intInRange (l, h) rng =
       l + Word.toInt (wordInRange (0w0, Word.fromInt (h - l)) rng)


   val pow53 = LargeWord.<< (0w1, 0w53)
   val pow53 = Real.fromLargeInt (LargeWord.toLargeInt pow53)
   val pow26 = LargeWord.<< (0w1, 0w26)
   val pow26 = Real.fromLargeInt (LargeWord.toLargeInt pow26)

   (* This is supposed to generate a real in the range [0, 1) with
    * 53-bit resolution. *)
   fun real rng = let
      val a = Real.fromInt (Word.toInt (Word.>> (RNG.rand rng, 0w5)))
      val b = Real.fromInt (Word.toInt (Word.>> (RNG.rand rng, 0w6)))
   in
      (a * pow26 + b) / pow53
   end

   fun realInRange (h, l) rng = l + (real rng * (h - l))

   (*
    * This performs the Box-Müller transform (in polar form) on
    * uniformly distributed random numbers.
    *
    * For more information on Box-Müller, see the Wikipedia page:
    *    http://en.wikipedia.org/wiki/Box-Muller_transform
    *)
   fun standardNormal rng = let
      fun try () = let
	 val u = real rng * 2.0 - 1.0
	 val v = real rng * 2.0 - 1.0
	 val s = u * u + v * v
      in
	 if s > 1.0 orelse s < Real.minPos then
	    try ()
	 else
	    u * Math.sqrt ((~2.0 * Math.ln s) / s)
      end
   in
      try ()
   end

   local
      val idChars = "abcdefghijklmnopqrstuvwxyz"
		    ^ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		    ^ "0123456789-_"
   in
      fun idString n rng = let
	 val g = intInRange (0, 64)
	 fun c _ = String.sub (idChars, g rng)
      in
	 CharVector.tabulate (n * 2, c)
      end
   end

end
