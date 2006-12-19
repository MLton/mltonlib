structure Factor =
  struct
    local
      open LargeInt
    in
      fun isPrime n =
        let
          fun chop (c, e) = if c mod 2 = 0 then chop (c div 2, e+1) else (c, e)
          val (c, e) = chop (n-1, 0)
          
          fun exp (x, 0) = 1
            | exp (x, 1) = x
            | exp (x, e) = 
              let val y = exp (x*x mod n, e div 2)
              in if e mod 2 = 0 then y else y * x mod n end
          
          fun checkPow (w, e) =
            e > 0 andalso (w+1 = n orelse checkPow (w*w mod n, e-1))
          fun millerRabin w = 
            let val v = exp (w, c)
            in v = 1 orelse checkPow (v, e) end
        in
          List.foldl (fn (w, a) => a andalso millerRabin (1 + w mod (n-1))) 
            true [62151, 7444, 40814, 49239, 71708759, 4481, 665652934 ]
        end
      
      fun factor n =
        let
          fun sort nil = nil
            | sort (p :: r) =
              let val (s, b) = List.partition (fn x => x < p) r
              in sort s @ p :: sort b end
          
          fun gcd (a, b) = if a = 0 then b else gcd (b mod a, a)
          
          fun findafactor n =
            let
              val start = Word.toLargeInt (MLton.Random.rand ()) mod n
              fun f x = (x*x + 2) mod n
              fun loop x y =
                let val g = gcd (n, n+x-y)
                in if g = 1 then loop (f x) (f (f y)) else g end
            in loop start (f start) end
          
          fun suck n l =
            if n = 1 then l else
            if isPrime n then n :: l else
            let val x = findafactor n
            in suck x (suck (n div x) l) end
        in
          sort (suck n [])
        end
    end
  end
