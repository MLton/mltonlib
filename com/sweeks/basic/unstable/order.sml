structure Order = struct

   datatype t = Equal | Greater | Less

   val ofBasis =
      fn EQUAL => Equal
       | GREATER => Greater
       | LESS => Less

(*    val toBasis =
 *       fn Equal => EQUAL
 *        | Greater => GREATER
 *        | Less => LESS
 *)

end

datatype order = datatype Order.t
