structure IntInf: INT_INF = struct

   open IntInf

   local
      structure S = Int (IntInf)
   in
      open S
   end

end
