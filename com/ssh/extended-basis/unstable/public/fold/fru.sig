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
   type ('value, 'rec) upd = 'value -> 'rec UnOp.t
   type ('args, 'upds, 'result) args =
        (('args, 'upds) t', ('args, 'upds) t', 'result) Fold.t

   val fru :
       (((('a -> Unit.t) * 'b UnOp.t, 'c, 'd, 'e, 'c) StaticSum.t,
         ('f, 'f, 'g, 'g,
          (('h -> 'rec UnOp.t) -> 'prod_upds) *
          ('h -> 'prod UnOp.t)) StaticSum.t,
         ('rec, 'prod) Iso.t -> ('upds, 'prod_upds) Iso.t ->
         (('rec, 'upds, 'rec) t, 's) CPS.t) Fold.t,
        't) CPS.t

   val fruData :
       ('data, 'rec) Iso.t ->
       (((('c -> Unit.t) * 'd UnOp.t, 'e, 'f, 'g, 'e) StaticSum.t,
         ('h, 'h, 'i, 'i,
          (('j -> 'rec UnOp.t) -> 'prod_upds) *
          ('j -> 'prod UnOp.t)) StaticSum.t,
         ('rec, 'prod) Iso.t -> ('upds, 'prod_upds) Iso.t ->
         (('rec, 'upds, 'data) t, 't) CPS.t) Fold.t,
        'u) CPS.t

   val args :
       (((('a -> Unit.t) * ('b -> 'b), 'c, 'd, 'e, 'c) StaticSum.t,
         ('f, 'f, 'g, 'g,
          (('h -> 'i -> 'j) -> 'k) * ('h -> 'l -> 'm)) StaticSum.t,
         ('i -> 'l) * ('m -> 'j) -> 'n * ('k -> 'o) -> 'p -> ('p -> 'q) ->
         ((('r, 'o) t', ('p, 's) t', 'q) Fold.t, 't) CPS.t) Fold.t,
        'u) CPS.t

   val A :
       ((('a * 'b, 'c UnOp.t * ('d -> 'e -> 'd),
          (('f -> 'g) -> 'h) * ('i -> 'j -> 'k),
          ((('f, 'l, 'm, 'l, 'l) StaticSum.t -> 'g) ->
           ('h, 'm -> 'g) Product.t) *
          (('i, ('j, 'n) Product.t -> ('k, 'n) Product.t,
            'o, ('p, 'q) Product.t -> ('p, 'o) Product.t,
            'r) StaticSum.t -> 'r), 's) StaticSum.t, 't, 'u) Fold.t,
        (('v, 'w, 's, 'x, 'x) StaticSum.t, 't, 'u) Fold.t, 'y) Fold.s

   val U :
       ('upds -> 'val -> 'rec UnOp.t) ->
       'val ->
       ((('rec, 'upds) t', ('rec, 'upds) t', 'result) Fold.t,
        (('rec, 'upds) t', ('rec, 'upds) t', 'result) Fold.t, 'k) Fold.s

   val D :
       'rec ->
       ((('rec, 'upds) t', ('rec, 'upds) t', 'result) Fold.t,
        (('rec, 'upds) t', ('rec, 'upds) t', 'result) Fold.t, 'k) Fold.s
end
