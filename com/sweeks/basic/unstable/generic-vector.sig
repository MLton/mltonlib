signature GENERIC_VECTOR = sig

   include SEQUENCE

   val make: int -> {done: unit -> 'a t0,
                     sub: int -> 'a elem,
                     update: int * 'a elem -> unit}

   structure Unsafe:
      sig
         val sub: 'a t0 * int -> 'a elem
      end

end


