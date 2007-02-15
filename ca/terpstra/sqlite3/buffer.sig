signature BUFFER =
   sig
      type 'a t
      val empty: unit -> 'a t
      val subOpt: 'a t * int -> 'a option
      val sub: 'a t * int -> 'a
      val push: 'a t * 'a -> int
   end
