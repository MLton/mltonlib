structure Seq = struct

   local
      datatype 'a t = T of Unit.t -> ('a * 'a t) Option.t
   in
      type 'a t = 'a t

      fun get (T f) = f ()

      fun delay f = T (get o f)
         
      fun empty () = T (fn () => None)
         
      fun cons (x, s) = T (fn () => Some (x, s))
   end

end

