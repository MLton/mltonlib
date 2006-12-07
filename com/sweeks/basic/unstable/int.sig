signature INT = sig

   include ORDERED

   val + : t * t -> t
   (**
    * i + j returns the sum of i and j.  It may raise Overflow.
    *)
   val - : t * t -> t
   (**
    * i - j returns the difference of i and j.  It may raise Overflow.
    *)
   val * : t * t -> t
   (**
    * i * j returns the product of i and j.  It may raise Overflow.
    *)
   val div: t * t -> t
   (**
    * i div j returns floor (i/j).  It may raise Overflow.
    * i div 0 raises Div.
    *)
   val fold: t * t * 'a * (t * 'a -> 'a) -> 'a
   (**
    * fold (start, stop, b, f) = f (stop-1, ... f (start+1, f (start, b)))
    *)
   val foldDown: t * t * 'a * (t * 'a -> 'a) -> 'a
   (**
    * fold (start, stop, b, f) = f (start, f (start+1, ... f (stop-1, b)))
    *)
   val for: t * t * (t -> unit) -> unit
   (**
    * for (start, stop, f) = (f start; f (start + 1); ...; f (stop-1))
    *)
   val fromTo: t * t -> t seq
   (**
    * fromTo (i, j) = fromToBy (i, j, 1)
    *)
   val fromToBy: t * t * t -> t seq
   (**
    * fromToBy (i, j, k) returns the sequence [i, i + k, i + 2k, ...], stopping
    * when j is reached.
    *)
   val geu: t * t -> bool
   (**
    * geu (i, j) = toWord i >= toWord j
    *)
   val mod: t * t -> t
   (**
    * i mod j returns the remainder of the division of i by j.
    * i mod j has the same sign as j.
    * i mod 0 raises Div.
    * i = j * (i div j) + i mod j.
    *)
   val ofString: string -> t option
   (**
    * ofString s = ofStringRadix (s, Radix.dec)
    *)
   val ofStringRadix: string * Radix.t -> t option
   (**
    * ofStringRadix (s, r) returns Some i if s is the representation of i in
    * radix r.
    *)
   val quot: t * t -> t
   (**
    * quot (i, j) returns the integer part of i/j.  It may raise Overflow.
    * quot (i, 0) raises Div.
    *)
   val rem: t * t -> t
   (**
    * rem (i, j) returns the remainder of the division of i by j.
    * i = j * quot (i, j) + rem (i, j).
    * rem (i, j) has the same sign as i.
    * rem (i, 0) raises Div.
    *)
   val scanner: Radix.t -> t Scanner.t
   (**
    * scanner r returns a scanner for ints where characters are interepreted
    * according to radix r.
    *)
   val toString: t -> string
   (**
    * toString i = toStringRadix (i, Radix.Dec)
    *)
   val toStringRadix: t * Radix.t -> string
   (**
    * toStringRadix (i, r) returns the string representation of i in radix r.
    *)
   val toWord: t -> word
   (**
    * toWord i converts i to a word using i's twos-complement representation.
    *)

end
