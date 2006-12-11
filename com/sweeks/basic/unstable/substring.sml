structure Substring: SUBSTRING = struct

   open VectorSlice

   type 'a elem = Char.t
   type t = Char.t t
   type 'a t0 = t
   type 'a base = String.t
   
end
