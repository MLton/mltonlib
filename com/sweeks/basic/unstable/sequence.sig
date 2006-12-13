(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature SEQUENCE = sig
   
   include SLICEABLE

   type ('a, 'b) unfold
   type ('a, 'b) unfoldR
   (**
    * Used for the return state of a call to unfold{,R}.
    * Lazy constructions define this as Unit.t.
    * Strict constructions define this as 'a.
    *)

   val append: 'a t0 * 'a t0 -> 'a t0
   (**
    * append ([x0, x1, ..., xn-1], [y0, y1, ..., ym-1]) =
    * [x0, x1, ..., xn-1, y0, y1, ..., ym-1].
    *)
   val concat: 'a t0 Seq.t -> 'a t0
   (**
    * concat ss returns the concatenation of all the sequences in s.
    *)
   val cons: 'a elem * 'a t0 -> 'a t0
   (**
    * cons (x, [x0, x1, ..., xn-1]) = [x, x0, x1, ..., xn]
    *)
   val drop: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * drop (s, f) returns the subsequence of s containing all x in s such that
    * not (f x).
    *)
   val empty: Unit.t -> 'a t0
   (**
    * empty () returns the empty sequence, i.e. [].
    *)
   val join: 'a t0 Seq.t * 'a t0 -> 'a t0
   (**
    * join (s, sep) creates a new sequence concatenating all the sequences in s,
    * separating the sequences by sep.
    *)
   val keep: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * keep (s, f) returns the subsequence of s containing all x in s such that
    * f x.
    *)
   val map: 'a t0 * ('a elem -> 'b elem) -> 'b t0
   (**
    * map (s, f) returns [f x0, f x1, ..., f xn-1], where
    * s = [x0, x1, ..., xn-1].
    *)
   val ofSeq: 'a elem Seq.t -> 'a t0
   (**
    * ofSeq s returns a sequence of the elements in s.  It is equivalent to
    * ofSeqN (s, Seq.size s).
    *)
   val ofSeqN: 'a elem Seq.t * Int.t -> 'a t0
   (**
    * ofSeqN (s, n) returns a sequence of size n consisting of the elements in
    * sequence s.
    *)
   val reverse: 'a t0 -> 'a t0
   (**
    * reverse s returns [xn-1, ..., x1, x0], where s = [x0, x1, ..., xn-1].
    *)     
   val single: 'a elem -> 'a t0
   (**
    * single x returns [x]
    *)
   val separate: 'a t0 * 'a elem -> 'a t0
   (**
    * separate (s, y) = [x0, y, x1, y, ..., y, xn-1],
    * where s = [x0, x1, ..., xn-1].
    *)
   val tabulate: Int.t * (Int.t -> 'a elem) -> 'a t0
   (**
    * tabulate (n, f) returns the sequence [f 0, f 1, ..., f (n-1)].
    * It may compute the elements in any order.
    *)
   val unfold: 'b * ('b -> ('a elem * 'b) Option.t) -> ('a t0, 'b) unfold
   (**
    * unfold (b0, f) = [a0, a1, ...] where f bi = Some (ai, bi+1)
    *)
   val unfoldN:
      Int.t * 'b * (Int.t * 'b -> ('a elem * 'b) Option.t) -> ('a t0, 'b) unfold
   (**
    * unfoldN (n, b0, f) = [a0, a1, ..., an-1]
    * where f (i, bi) = Some (ai, bi+1)
    *)
   val unfoldNR:
      Int.t * 'b * (Int.t * 'b -> ('a elem * 'b) Option.t)
      -> ('a t0, 'b) unfoldR
   (**
    * unfoldNR (n, b0, f) = [an-1, ..., a1, a0]
    * where f (n - 1 - i, bi) = Some (ai, bi+1)
    *)
   val unfoldR: 'b * ('b -> ('a elem * 'b) Option.t) -> ('a t0, 'b) unfoldR
   (**
    * unfoldR (b0, f) = [an-1, ..., a1, a0] where f bi = Some (ai, bi+1) and
    * f bn = None.
    *)

end
