(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature ENUMERABLE = sig

   type 'a elem
   type 'a t0
   (* Can't use type "t", because this signature needs to be specialized to
    * both monomorphic and polymorphic instances, and for the monomorphic ones,
    * we would like to call the nullary tycon "t".  Sadly, SML doesn't allow
    * overriding in signatures.
    *)

   val all: 'a t0 * ('a elem -> Bool.t) -> Bool.t
   (**
    * all (s, f) returns true iff for all x in s, f x.
    *)
   val exists: 'a t0 * ('a elem -> Bool.t) -> Bool.t
   (**
    * exists (s, f) returns true iff f x for some x in s.
    *)
   val find: 'a t0 * ('a elem -> Bool.t) -> 'a elem Option.t
   (**
    * find (s, f) returns Some x if x is the first element of s such that f x.
    * find (s, f) returns None if not (f x) for all x in s.
    *)
   val fold: 'a t0 * 'b * ('a elem * 'b -> 'b) -> 'b
   (**
    * fold ([], b, f) = b
    * fold (x :: s, b, f) = fold (s, f (x, b), f)
    *)
   val for: 'a t0 * ('a elem -> Unit.t) -> Unit.t
   (**
    * for ([x0, ..., xn-1], f) = (f x0; f x1; ...; f (xn-1))
    *)
   val isEmpty: 'a t0 -> Bool.t
   (**
    * isEmpty s = size s = 0
    *)
   val last: 'a t0 -> 'a elem
   (**
    * last [x0, ..., xn-1] = xn-1
    * last [] raises an exception.
    *)
   val recur:
      'a t0 * 'b * ('b -> 'c) * ('a elem * 'b * ('b -> 'c) -> 'c) -> 'c
   (**
    * recur ([], b, f, g) = f b
    * recur (x :: s, b, f, g) = recur (s, g (x, b, fn b => recur (s, b, f, g)))
    *)
   val size: 'a t0 -> Int.t
   (**
    * size s returns the number of elements in s.
    *)
   val sub: 'a t0 * Int.t -> 'a elem
   (**
    * sub (s, i) returns the i'th element of s.
    *)
   val toSeq: 'a t0 -> 'a elem Seq.t
   (**
    * toSeq.T s returns a sequence of the elements in s.
    *)
   val toSeqR: 'a t0 -> 'a elem Seq.t
   (**
    * toSeqR s returns a sequence of the elements in s in reverse order.
    * It is more efficient that Seq.reverse o toSeq.
    *)

end
