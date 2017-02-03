structure Subsets :> SUBSETS = struct

   fun kSubsetsInt (n, k) =
       if k = 0 orelse n < k then
	  [[]]
       else if n = k then
	  [List.tabulate (n, fn x => x)]
       else
	  (map (fn l => (n - 1) :: l) (kSubsetsInt (n - 1, k - 1)))
	  @ kSubsetsInt (n - 1, k)

   fun mapSet f (l, k) =
       map (fn il => map (fn i => List.nth (l, i)) il)
	   (f (length l, k))

   fun kSubsets (l, k) = mapSet kSubsetsInt (l, k)

   fun kMultisetsInt (n, k) = let
      fun loop (left, min) =
	  if left = 0 then
	     List.tabulate (n - min, fn i => [min + i])
	  else
	     List.concatMap (fn i => map (fn l => i :: l) (loop (left - 1, i)))
			    (List.tabulate (n - min, fn i => min + i))
   in
      loop (k - 1, 0)
   end

   fun kMultisets (l, k) = mapSet kMultisetsInt (l, k)

end
