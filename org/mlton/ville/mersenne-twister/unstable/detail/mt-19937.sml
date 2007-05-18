structure MersenneTwister : RNG = struct
   open Word32
   val && = andb
   val || = orb
   val op^ = xorb
   infix >> << && ||

   val n = 0w624
   val m = 0w397

   datatype t = D of {mt: word array, i: word ref}

   fun new seed = let
      val mt = Array.array(toIntX n, 0w0)
      val _ = Array.update (mt, 0, seed)
      fun loop (prev, i) =
          if i < n then
             let
                val v = 0w1812433253 * (prev ^ (prev >> 0w30)) + i
             in
                Array.update (mt, toIntX i, v)
              ; loop (v, i + 0w1)
             end
          else i
      val i = loop (seed, 0w1)
   in
      D {mt = mt, i = ref i}
   end

   fun twist (mt, i, a, b) = let
      val y1 = Array.sub (mt, toIntX a) && 0wx80000000
      val y2 = Array.sub (mt, toIntX b) && 0wx7fffffff
      val y = y1 || y2
      val v = (y >> 0w1) ^ (if (y && 0w1) = 0w0 then 0w0 else 0wx9908b0df)
   in
      Array.update (mt, toIntX a, Array.sub (mt, toIntX i) ^ v)
   end

   fun generate mt = let
      fun loop (k, max, add) =
          if k < max then
             (twist (mt, k + add, k, k + 0w1)
            ; loop (k + 0w1, max, add))
          else ()
   in
      loop (0w0, n - m, m)
    ; loop (n - m, n - 0w1, m - n)
    ; twist (mt, m - 0w1, n - 0w1, 0w0)
    ; 0w0
   end

   fun temper y = let
      val y = y ^ (y >> 0w11)
      val y = y ^ ((y << 0w7) && 0wx9d2c5680)
      val y = y ^ ((y << 0w15) && 0wxefc60000)
      val y = y ^ (y >> 0w18)
   in y end

   fun rand (D self) = let
      val i = !(#i self)
      val mt = #mt self
      val i = if i >= n then generate mt else i
      val y = temper (Array.sub (mt, toIntX i))
   in
      (#i self) := i + 0w1
    ; y
   end

end
