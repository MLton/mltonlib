signature SCANNER = sig

   type 'a t
   (**
    * A scanner is a function that can extract a value from a prefix of a
    * sequence.
    *)

   val make: (Char.t Seq.t -> ('a * Char.t Seq.t) Option.t) -> 'a t
   val map: 'a t * ('a -> 'b) -> 'b t
   val scan: 'a t * Char.t Seq.t -> ('a * Char.t Seq.t) Option.t
   (**
    * scan (s, cs) runs scanner s on the sequence cs.
    *)

end
