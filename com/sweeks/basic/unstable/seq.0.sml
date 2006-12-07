structure Seq = struct

   local
      datatype 'a t = T of unit -> ('a * 'a t) option
   in
      type 'a t = 'a t

      fun get (T f) = f ()

      fun delay f = T (get o f)
         
      fun empty () = T (fn () => None)
         
      fun cons (x, s) = T (fn () => Some (x, s))
   end

end

type 'a seq = 'a Seq.t
