val () = let
   open Generic UnitTest Histogram

   fun opti (l, num) = let
      val v = Vector.fromList l
      val e = sseFromVector v
      open Cvt
   in
      optimal {error = (fn (a, b) =>
			   (prints [" err (", D a, ", ", D b, ") = "]
			  ; println (G (e (a, b)))
			  ; e (a, b))),
	       range = Vector.length v,
	       num = num}
   end

   fun err (l, bl) = let
      val ex = sseFromVector (Vector.fromList l)
      open Cvt
      val e = (fn (a, b) =>
		  (prints ["*err (", D a, ", ", D b, ") = "]
		 ; println (G (ex (a, b)))
		 ; ex (a, b)))
      fun sum (i, (prev, total)) = (i, e (prev, i) + total)
   in
      if length l = 0 then
	 0.0
      else
	 #2 (foldl sum (0, 0.0) (bl @ [length l]))
   end

in
   unitTests
      (title "Total error of bucketing")

      (testAll (tuple2 (list real, int))
	       (fn (l, n) => let
		      open Cvt
		      val () = if length l < 3 then skip () else ()
		      val () = if n < 2 then skip () else ()
		      val () = printlns ["l = ", show (list real) l, ", n = " ^ D n]
		      val n = 1 + n mod (length l - 1)
		      val (e, bl) = opti (l, n)
		   in
		      printlns ["n' = ", D n, ", e = ", G e, ", e' = ", G (err (l, bl))]
		    ; println ""
		    ; that (Real.abs (e - err (l, bl)) <= Real.minPos)
		   end))

      $
end
