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
   type ('a, 'b, 'c, 'd) f = (('a, 'b, 'c) t -> 'd) -> 'd
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g) s = ('a, 'b, 'c) t -> ('d, 'e, 'f, 'g) f
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) s1 =
        ('b, 'c, 'd) t -> 'a -> ('e, 'f, 'g, 'h) f

   val $ : ('a, 'a, 'b) t -> 'b

   val wrap : 'a * ('b -> 'c) -> ('a, 'b, 'c, 'd) f
   val unwrap : ('a, 'b, 'c, 'a * ('b -> 'c)) f -> 'a * ('b -> 'c)
   val rewrap : ('a, 'b, 'c, 'a * ('b -> 'c)) f -> ('a, 'b, 'c, 'd) f

   val post : ('a -> 'b)
              -> ('c, 'd, 'a, 'c * ('d -> 'a)) f
              -> ('c, 'd, 'b, 'e) f

   val map : ('a * ('b -> 'c) -> 'd * ('e -> 'f))
             -> ('a, 'b, 'c, 'd, 'e, 'f, 'g) s
   val unmap : ('a, 'b, 'c, 'd, 'd, 'd * ('e -> 'f), 'd * ('e -> 'f)) s
               -> 'a * ('b -> 'c) -> 'd * ('e -> 'f)
   val remap : ('a, 'b, 'c, 'd, 'd, 'd * ('e -> 'f), 'd * ('e -> 'f)) s
               -> ('a, 'b, 'c, 'd, 'e, 'f, 'g) s

   val map1 : ('a -> 'b * ('c -> 'd) -> 'e * ('f -> 'g))
              -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) s1
   val unmap1 : ('a, 'b, 'c, 'd, 'e, 'e, 'e * ('f -> 'g), 'e * ('f -> 'g)) s1
                -> 'a -> 'b * ('c -> 'd) -> 'e * ('f -> 'g)
   val remap1 : ('a, 'b, 'c, 'd, 'e, 'e, 'e * ('f -> 'g), 'e * ('f -> 'g)) s1
                -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) s1

   val mapFin : (('a -> 'b) -> 'c -> 'd) -> ('e, 'a, 'b, 'e, 'c, 'd, 'f) s
   val mapSt : ('a -> 'b) -> ('a, 'c, 'd, 'b, 'c, 'd, 'e) s
   val mapSt1 : ('a -> 'b1 -> 'b2) -> ('a, 'b1, 'c, 'd, 'b2, 'c, 'd, 'e) s1

   val comFinL : ('a -> 'b) -> ('c, 'd, 'a, 'c, 'd, 'b, 'e) s
   val comFinR : ('a -> 'b) -> ('c, 'b, 'd, 'c, 'a, 'd, 'e) s

   val comStL : ('a -> 'b) -> ('c -> 'a, 'd, 'e, 'c -> 'b, 'd, 'e, 'f) s
   val comStR : ('a -> 'b) -> ('b -> 'c, 'd, 'e, 'a -> 'c, 'd, 'e, 'f) s

   val comStL1 : ('a -> 'b -> 'c)
                 -> ('a, 'd -> 'b, 'e, 'f, 'd -> 'c, 'e, 'f, 'g) s1
   val comStR1 : ('a -> 'b -> 'c)
                 -> ('a, 'c -> 'd, 'e, 'f, 'b -> 'd, 'e, 'f, 'g) s1

   structure NSZ : sig
      type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t
      val wrap : {none : 'a -> 'b, some : 'c -> 'd, zero : 'e}
                 -> (('e, 'f, 'g, 'h, 'i, 'f, 'g) t,
                     ('j, 'a, 'b, 'c, 'd, 'j, 'k) t,
                     'k, 'l) f
      val mapSt : {none : 'a -> 'b, some : 'c -> 'd}
                  -> (('e, 'a, 'b, 'c, 'd, 'e, 'f) t, 'g, 'h,
                      ('f, 'i, 'j, 'k, 'l, 'k, 'l) t, 'g, 'h, 'm) s
      val mapSt1 : {none : 'a -> 'b, some : 'c -> 'd}
                   -> ('e,
                       ('f, 'a, 'b, 'c, 'd, 'e, 'f -> 'g) t, 'h, 'i,
                       ('g, 'j, 'k, 'l, 'm, 'l,       'm) t, 'h, 'i, 'n) s1
   end
end
