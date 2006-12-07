signature SEQ = sig

   type 'a t

   include SEQUENCE where type 'a t0 = 'a t

   val delay: (unit -> 'a t) -> 'a t
   (**
    * delay f returns a sequence that is like f(), except that f() is not
    * computed until the sequence is needed.
    *)
   val get: 'a t -> ('a * 'a t) option
   (**
    * get [] = None
    * get [x0, x1, ..., xn-1] = (x0, [x1, ..., xn-1])
    *)

end
