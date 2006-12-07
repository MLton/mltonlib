structure Array = struct

   type 'a t = 'a array

   local
      open Array
   in
      val make = array
      val size = length
      val sub = sub
      val update = update
   end

   structure Unsafe = struct
      fun make n =
         if MLton.safe andalso n < 0 then
            raise Size
         else
            Primitive.Array.array n
      val sub = Unsafe.Array.sub
      val update = Unsafe.Array.update
   end

end
