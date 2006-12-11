signature GENERIC_VECTOR = sig

   include SEQUENCE

   val make: Int.t -> {done: Unit.t -> 'a t0,
                     sub: Int.t -> 'a elem,
                     update: Int.t * 'a elem -> Unit.t}

   structure Unsafe:
      sig
         val sub: 'a t0 * Int.t -> 'a elem
      end

end


