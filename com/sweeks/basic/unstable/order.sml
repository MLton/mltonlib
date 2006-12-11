structure Order = struct

   datatype t = Equal | Greater | Less

   local
      datatype z = datatype Basis.Order.t
   in
      val ofBasis =
         fn EQUAL => Equal
          | GREATER => Greater
          | LESS => Less
   end
(*    val toBasis =
 *       fn Equal => EQUAL
 *        | Greater => GREATER
 *        | Less => LESS
 *)

end
