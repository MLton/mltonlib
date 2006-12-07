signature REF = sig

   datatype t = datatype ref
   (**
    * SML's reference type.
    *)

   val ! : 'a t -> 'a
   (**
    * !r returns the value stored in r.
    *)
   val := : 'a t * 'a -> unit
   (**
    * r := v changes the value in r to v.
    *)

end
