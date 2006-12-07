structure Lazy: LAZY = struct

   fun memo th = let
      val r = ref None
   in
      fn () =>
      case !r of
         None => let
            val a = th ()
            val () = r := Some a
         in
            a
         end
      | Some a => a
   end

end

val lazy = Lazy.memo
