functor ProbabilityVector (type item
                           val items : item vector)
	: PROBABILITY_VECTOR = struct
   type t = real vector
   type item = item
   val items = items
   val size = Vector.length items
   fun item i = Vector.sub (items, i)
   fun make p = Vector.tabulate (size, fn _ => p)
   fun tabulate f = Vector.tabulate (size, f o item)
   fun map (v, f) = Vector.mapi (fn (i, p) => f (p, item i)) v
   fun app (v, f) = Vector.appi (fn (i, p) => f (p, item i)) v
   fun toList v = ListPair.zip (Vector.toList v, Vector.toList items)
   val toVector = id
   fun get (v : t, i) = Vector.sub (v, i)
   fun mul (a, b) = Vector.tabulate (size, (fn i => get (a, i) * get (b, i)))
   fun fold (v, f, z) = Vector.foldli (fn (i, p, v) => f (p, item i, v)) z v
   val fromArray = Array.vector

   fun normalize v = let
      val c = 1.0 / Vector.foldl Real.+ 0.0 v
   in
      Vector.map (fn p => p * c) v
   end

end
