(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Support for functional record update.
 *
 * See [http://mlton.org/FunctionalRecordUpdate FRU] for further
 * information.
 *)
signature FRU = sig
   type ('rec, 'upds) t'
   type ('rec, 'upds, 'data) t =
        (('rec, 'upds) t', ('rec, 'upds) t', 'data UnOp.t) Fold.t

   val fru :
       (((('a -> unit) * 'b UnOp.t, 'c, 'd, 'e, 'f, 'c, 'd) Fold.NSZ.t',
         ('g,
          (('h -> 'i UnOp.t) -> 'j) * ('h -> 'k UnOp.t),
          ('i, 'k) Iso.t -> ('l, 'j) Iso.t -> 'l,
          (('m -> 'n UnOp.t) -> 'o) * ('m -> 'p UnOp.t),
          ('n, 'p) Iso.t -> ('q, 'o) Iso.t -> 'q,
          'g,
          'r -> 's -> 'upds) Fold.NSZ.t',
         'r -> 's ->
         (('rec, 'upds, 'rec) t, 'v) CPS.t) Fold.t,
        'w) CPS.t

   val fruData :
       ('data, 'rec) Iso.t ->
       (((('c -> unit) * 'd UnOp.t, 'e, 'f, 'g, 'h, 'e, 'f) Fold.NSZ.t',
         ('i,
          (('j -> 'k UnOp.t) -> 'l) * ('j -> 'm UnOp.t),
          ('k, 'm) Iso.t -> ('n, 'l) Iso.t -> 'n,
          (('o -> 'p UnOp.t) -> 'q) * ('o -> 'r UnOp.t),
          ('p, 'r) Iso.t -> ('s, 'q) Iso.t -> 's,
          'i,
          't -> 'u -> 'upds) Fold.NSZ.t',
         't -> 'u ->
         (('rec, 'upds, 'data) t, 'w) CPS.t) Fold.t,
        'x) CPS.t

   val A :
       ((('a,
          'b * 'c,
          'd UnOp.t * ('e -> 'f -> 'e),
          (('g -> 'h) -> 'i) * ('j -> 'k UnOp.t),
          ((('g, 'l) Sum.t -> 'h) -> ('i, 'l -> 'h) Product.t) *
          (('j, 'm) Sum.t -> ('k, 'm) Product.t UnOp.t),
          'a,
          'n) Fold.NSZ.t',
         'o,
         'p) Fold.t,
        (('n, 'q, 'r, 's, 't, 's, 't) Fold.NSZ.t', 'o, 'p) Fold.t, 'u) Fold.s

   val U :
       ('upds -> 'val -> 'rec UnOp.t) ->
       'val ->
       (('rec, 'upds, 'data) t,
        ('rec, 'upds, 'data) t, 'k) Fold.s
end
