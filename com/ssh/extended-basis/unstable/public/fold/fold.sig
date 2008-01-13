(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for combinators for defining variable arity functions.
 *
 * See also: [http://mlton.org/Fold]
 *)
signature FOLD = sig
   type ('a, 'b, 'c) t
   type ('s1, 's2, 'r) s = 's1 -> ('s2, 'r) CPS.t
   type ('a, 's1, 's2, 'r) s1 = 's1 -> 'a -> ('s2, 'r) CPS.t

   val $ : ('a, 'a, 'b) t -> 'b

   val wrap : 'a * ('b -> 'c) -> (('a, 'b, 'c) t, 'd) CPS.t
   val unwrap : (('a, 'b, 'c) t, 'a * ('b -> 'c)) CPS.t -> 'a * ('b -> 'c)
   val rewrap : (('a, 'b, 'c) t, 'a * ('b -> 'c)) CPS.t ->
                (('a, 'b, 'c) t, 'd) CPS.t

   val post : ('a -> 'b)
              -> (('c, 'd, 'a) t, 'c * ('d -> 'a)) CPS.t
              -> (('c, 'd, 'b) t, 'e) CPS.t

   val map : ('a * ('b -> 'c) -> 'd * ('e -> 'f))
             -> (('a, 'b, 'c) t, ('d, 'e, 'f) t, 'g) s
   val unmap : (('a, 'b, 'c) t, ('d, 'd, 'd * ('e -> 'f)) t, 'd * ('e -> 'f)) s
               -> 'a * ('b -> 'c) -> 'd * ('e -> 'f)
   val remap : (('a, 'b, 'c) t, ('d, 'd, 'd * ('e -> 'f)) t, 'd * ('e -> 'f)) s
               -> (('a, 'b, 'c) t, ('d, 'e, 'f) t, 'g) s

   val map1 : ('a -> 'b * ('c -> 'd) -> 'e * ('f -> 'g))
              -> ('a, ('b, 'c, 'd) t, ('e, 'f, 'g) t, 'h) s1
   val unmap1 : ('a, ('b, 'c, 'd) t, ('e, 'e, 'e * ('f -> 'g)) t, 'e * ('f -> 'g)) s1
                -> 'a -> 'b * ('c -> 'd) -> 'e * ('f -> 'g)
   val remap1 : ('a, ('b, 'c, 'd) t, ('e, 'e, 'e * ('f -> 'g)) t, 'e * ('f -> 'g)) s1
                -> ('a, ('b, 'c, 'd) t, ('e, 'f, 'g) t, 'h) s1

   val mapFin : (('a -> 'b) -> 'c -> 'd) -> (('e, 'a, 'b) t, ('e, 'c, 'd) t, 'f) s

   val mapSt : ('a -> 'b) -> (('a, 'c, 'd) t, ('b, 'c, 'd) t, 'e) s
   val mapSt1 : ('a -> 'b1 -> 'b2) -> ('a, ('b1, 'c, 'd) t, ('b2, 'c, 'd) t, 'e) s1

   val comFinL : ('a -> 'b) -> (('c, 'd, 'a) t, ('c, 'd, 'b) t, 'e) s
   val comFinR : ('a -> 'b) -> (('c, 'b, 'd) t, ('c, 'a, 'd) t, 'e) s

   val comStL : ('a -> 'b) -> (('c -> 'a, 'd, 'e) t, ('c -> 'b, 'd, 'e) t, 'f) s
   val comStR : ('a -> 'b) -> (('b -> 'c, 'd, 'e) t, ('a -> 'c, 'd, 'e) t, 'f) s

   val comStL1 : ('a -> 'b -> 'c)
                 -> ('a, ('d -> 'b, 'e, 'f) t, ('d -> 'c, 'e, 'f) t, 'g) s1
   val comStR1 : ('a -> 'b -> 'c)
                 -> ('a, ('c -> 'd, 'e, 'f) t, ('b -> 'd, 'e, 'f) t, 'g) s1

   structure NSZ : sig
      type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t'
      val wrap : {none : 'a -> 'b, some : 'c -> 'd, zero : 'e}
                 -> ((('e, 'f, 'g, 'h, 'i, 'f, 'g) t',
                      ('j, 'a, 'b, 'c, 'd, 'j, 'k) t',
                      'k) t, 'l) CPS.t
      val mapSt : {none : 'a -> 'b, some : 'c -> 'd}
                  -> ((('e, 'a, 'b, 'c, 'd, 'e, 'f) t', 'g, 'h) t,
                      (('f, 'i, 'j, 'k, 'l, 'k, 'l) t', 'g, 'h) t, 'm) s
      val mapSt1 : {none : 'a -> 'b, some : 'c -> 'd}
                   -> ('e,
                       (('f, 'a, 'b, 'c, 'd, 'e, 'f -> 'g) t', 'h, 'i) t,
                       (('g, 'j, 'k, 'l, 'm, 'l,       'm) t', 'h, 'i) t, 'n) s1
   end
end
