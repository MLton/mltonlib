signature GENERIC_ARRAY = sig

   include SEQUENCE

   val make: int * 'a -> 'a t0
   (**
    * make (n, x) creates an array of size n where all elements are x.
    *)
   val update: 'a t0 * int * 'a -> unit
   (**
    * update (a, i, x) sets the i'th entry of array a to be x.
    *)
   val updates: 'a t0 * int * 'a seq -> unit
   (**
    * updates (a, i, s) updates the entries of array a starting at index i and
    * the first element of sequence s, continuing with subsequent indices
    * and elements of s.
    *)

   structure Unsafe:
      sig
         val make: int -> 'a t0
         (**
          * make n creates an uninitialized array of size n.  This is unsafe
          * and you must update each element of the array before using it.
          *)
         val sub: 'a t0 * int -> 'a
         (**
          * sub (a, i) returns the i'th entry of array a.  It does not check
          * that 0 <= i < size a.
          *)
         val update: 'a t0 * int * 'a -> unit
         (**
          * udpate (a, i, x) sets the i'th entry of array a to be x.  It does
          * not check that 0 <= i < size a.
          *)
      end

end
