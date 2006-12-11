signature GENERIC_SLICE = sig

   type 'a base

   include SLICEABLE

   val base: 'a t0 -> 'a base * {start: Int.t}
   (**
    * base s returns the sequence of which s is a slice.
    *)
   val full: 'a base -> 'a t0
   (**
    * full b returns a slice containing all the elements of b.
    *)
   val get: 'a t0 -> ('a elem * 'a t0) Option.t
   (**
    * get [] = None
    * get [x0, x1, ..., xn-1] = (x0, [x1, ..., xn-1])
    *)
   val map: 'a t0 * ('a elem -> 'b elem) -> 'b base
   (**
    * map (s, f) returns the base sequence [f x0, f x1, ..., f xn-1], where
    * s = [x0, x1, ..., xn-1].
    *)
   val slice: 'a t0 * {size: Int.t, start: Int.t} -> 'a t0
   (**
    * slice (a, {size = n, start = i}) returns the subsequence of s starting
    * at index i and of length n.
    *)

end
