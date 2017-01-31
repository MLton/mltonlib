(* This implementation is adapted directly from [1].  Only the naïve
 * version of the algorithm is currently implemented, so this is
 * is slow/unusable for large inputs.
 *
 * References:
 *   [1] H. V. Jagadish, Nick Koudas, S. Muthukrishnan, Viswanath Poosala,
 *   Kenneth C. Sevcik, Torsten Suel: Optimal Histograms with Quality Guarantees,
 *   Proc. 24th Int. Conf. Very Large Data Bases, 1998
 *   http://infolab.stanford.edu/~datar/courses/cs361a/papers/vopt.pdf
 *)

structure Histogram :> HISTOGRAM = struct
   type e = int * int -> real


   fun optimal {error, num, range} = let
      val tbl = Array.tabulate
		   (range, fn _ => Array.tabulate (num, fn _ => NONE))
      fun sub (j, k) = valOf (Array.sub (Array.sub (tbl, j), k))
      fun set (j, k, v) = Array.update (Array.sub (tbl, j), k, SOME v)
      fun doOne (i, k) = let
	 fun best (j, e, l) =
	     if j >= i then
		(e, l)
	     else
		let
		   val (eC, lC) = sub (j, k - 1)
		   val eC = eC + error (j + 1, i)
		in
		   if eC < e then
		      best (j + 1, eC, j + 1 :: lC)
		   else
		      best (j + 1, e, l)
		end
      in
	 if k = 0 then
	    set (i, k, (error (0, i), []))
	 else
	    set (i, k, best (0, Real.posInf, []))
      end
      fun allI (k, x) =
	  if x >= range then () else (doOne (x, k); allI (k, x + 1))
      fun allK x = if x >= num then () else (allI (x, 0); allK (x + 1))
   in
      if range = 0 then
	 (0.0, [])
      else
	 (allK 0
	; let
	   val (e, l) = sub (range - 1, num - 1)
	in
	   (e, rev l)
	end)
   end

   fun sse (n, f) = let
      fun sum (i, s) = if i = n then (s, s) else (s, s + f i)
      fun sumSquared (i, s) = if i = n then (s, s) else (s, s + f i * f i)
      val (p, _) = MLton.Vector.unfoldi (n + 1, 0.0, sum)
      val (pp, _) = MLton.Vector.unfoldi (n + 1, 0.0, sumSquared)
   in
      fn (i, j) => let
	    val fk2 = Vector.sub (pp, j + 1) - Vector.sub (pp, i)
	    val d = Real.fromInt (j - i + 1)
	    val avg = Vector.sub (p, j + 1) - Vector.sub (p, i)
	 in
	    if i = j then
	       0.0
	    else
	       fk2 - (avg * avg) / d
	 end
   end

   fun sseFromVector v = sse (Vector.length v, fn i => Vector.sub (v, i))
   fun sseFromArray a = sse (Array.length a, fn i => Array.sub (a, i))

end
