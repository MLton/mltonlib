functor BitField (W : WORD) :> BIT_FIELD_W where type word = W.t = struct
   type t = W.t
   type word = W.t

   local open Generic in
     val t = iso largeWord (fn w => W.toLargeWord w,
	  		    fn w => W.fromLargeWord w)
   end

   val zero = W.fromInt 0
   val one = W.fromInt 1
   val allOne = W.notb zero
   val size = W.wordSize
   val fromWord = id
   val toWord = id

   fun make bit = if bit then W.notb zero else zero
   fun flip (bf, i) = W.xorb (bf, W.<< (one, Word.fromInt i))
   fun get (bf, i) = W.andb (bf, W.<< (one, Word.fromInt i)) <> zero
   fun allZero bf = bf = zero
   fun toString (bf, i) = StringCvt.padLeft #"0" i (W.fmt StringCvt.BIN bf)

   fun fromVector (v, b) =
       Vector.foldli (fn (i, b, bf) => if b then flip (bf, i) else bf)
		     (make b) v

   fun findOne (bf, i) = let
      fun loop (bf, i) = let
	 val s = W.>> (bf, i)
      in
	 if s = zero then
	    loop (bf, 0w0)
	 else if W.andb (s, one) <> zero then
	    i
	 else
	    loop (bf, i + 0w1)
      end
   in
      Word.toIntX (loop (bf, Word.fromInt i + 0w1))
   end

   fun count bf = let
      fun loop (bf, n) =
	  if bf = zero then
	     n
	  else
	     loop (W.>> (bf, 0w1),
		   if W.andb (bf, one) <> zero then n + 1 else n)
   in
      loop (bf, 0)
   end

   fun foldi f z bf = let
      fun loop (bf, i, v) =
	  if bf = zero then
	     v
	  else
	     loop (W.>> (bf, 0w1), i + 1, f (i, W.andb (bf, one) <> zero, v))
   in
      loop (bf, 0, z)
   end

   fun toVector bf = let
      fun f (i, b, l) = b :: l
   in
      Vector.fromList (rev (foldi f [] bf))
   end

end
