signature SCANNER = sig

   type 'a t
   (**
    * A scanner is a function that can extract a value from a prefix of a
    * sequence.
    *)

   val make: (char seq -> ('a * char seq) option) -> 'a t
   val map: 'a t * ('a -> 'b) -> 'b t
   val scan: 'a t * char seq -> ('a * char seq) option
   (**
    * scan (s, cs) runs scanner s on the sequence cs.
    *)

end
