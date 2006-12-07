structure IoDesc: IO_DESC = struct

   open OS.IO

   structure Kind = struct
      open OS.IO.Kind

      type t = iodesc_kind
         
      val == = op =
   end
      
   type t = iodesc

   val compare = Order.ofBasis o compare

end
