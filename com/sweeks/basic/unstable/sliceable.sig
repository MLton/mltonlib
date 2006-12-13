(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature SLICEABLE = sig
   
   include ENUMERABLE

   val dropPrefix: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * dropPrefix (s, f) returns the suffix of s obtained by dropping the maximal
    * prefix all of whose elements satisfy f.
    *)
   val dropPrefixN: 'a t0 * Int.t -> 'a t0
   (**
    * dropPrefixN (s, n) returns the suffix of s obtained by dropping its first
    * n elements.  It raises an exception if size s < n.
    *)
   val dropSuffix: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * dropSuffix (s, f) returns the prefix of s obtained by dropping the maximal
    * suffix all of whose elements satisfy f.
    *)
  val dropSuffixN: 'a t0 * Int.t -> 'a t0
   (**
    * dropSuffixN (s, n) returns the prefix of s obtained by dropping its last n
    * elements.  It raises an exception if size s < n.
    *)
   val fields: 'a t0 * ('a elem -> Bool.t) -> 'a t0 Seq.t
   (**
    * fields (s, f) breaks s Int.to a sequence of subsequences ss such that
    * each element of ss is a maximal subsequence of s not containing any
    * element x where f x.
    *)
   val keepPrefix: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * keepPrefix (s, f) returns the maximal prefix of s all of whose elements
    * satisfy f.
    *)
   val keepPrefixN: 'a t0 * Int.t -> 'a t0
   (**
    * keepPrefixN (s, n) returns the prefix of s of size n.  It raises an
    * exception if size s < n.
    *)
   val keepSuffix: 'a t0 * ('a elem -> Bool.t) -> 'a t0
   (**
    * keepSuffix (s, f) returns the maximal suffix of s all of whose elements
    * satisfy f.
    *)
   val keepSuffixN: 'a t0 * Int.t -> 'a t0
   (**
    * keepSuffixN (s, n) returns the suffix of s of size n.  It raises an
    * exception if size s < n.
    *)
   val splitPrefix: 'a t0 * ('a elem -> Bool.t) -> 'a t0 * 'a t0
   (**
    * splitPrefix (s, f) = (s1, s2)
    * splits s Int.to two subsequence such that s = append (s1, s2).
    * s1 is the maximal prefix of s such that for all x in s1, f x.
    *)
   val tokens: 'a t0 * ('a elem -> Bool.t) -> 'a t0 Seq.t
   (**
    * tokens (s, f) breaks s Int.to a sequence of subsequences ss such that
    * each element of ss is a nonempty maximal subsequence of s not containing
    * any element x where f x.
    *)

end
