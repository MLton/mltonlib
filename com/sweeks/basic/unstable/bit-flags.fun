functor BitFlags (S: BASIS_BIT_FLAGS): BIT_FLAGS = struct

   open S

   type t = flags

   fun difference (f1, f2) = clear (f2, f1)
   val doIntersect = anySet
   val isSubset = allSet
   val union = flags

end
