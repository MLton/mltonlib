(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Utility module for creating folds (see fold.sml) that need to treat the
 * cases of 0 and 1 or more steps differently.
 *
 * See
 *
 *   http://mlton.org/Fold01N
 *
 * for discussion.
 *)

signature FOLD01N = sig
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g) ac

   val fold : {none: 'a -> 'b,
               some: 'c -> 'd,
               zero: 'e}
              -> (('e, 'f, 'g, 'h, 'i, 'f, 'g) ac,
                  ('j, 'a, 'b, 'c, 'd, 'j, 'k) ac,
                  'k, 'l) Fold.t
   val step0 : {none: 'a -> 'b,
                some: 'c -> 'd}
               -> (('e, 'a, 'b, 'c, 'd, 'e, 'f) ac,
                   ('f, 'g, 'h, 'i, 'j, 'i, 'j) ac,
                   'k, 'l, 'm) Fold.step0
   val step1 : {none: 'a -> 'b,
                some: 'c -> 'd}
               -> ('e,
                   ('f, 'a, 'b, 'c, 'd, 'e * 'f, 'g) ac,
                   ('g, 'h, 'i, 'j, 'k, 'j, 'k) ac,
                   'l, 'm, 'n) Fold.step1
end

structure Fold01N :> FOLD01N = struct
   datatype ('a, 'b, 'c, 'd, 'e, 'f, 'g) ac =
            IN of 'a * (('b -> 'c) * ('d -> 'e) -> 'f -> 'g)

   fun fold {zero, none, some} =
       Fold.fold (IN (zero, Pair.fst),
                  fn IN (ac, pick) =>
                     pick (none, some) ac)

   fun step0 {none, some} =
       Fold.step0 (fn IN (ac, pick) =>
                      IN (pick (none, some) ac,
                          Pair.snd))

   fun step1 {none, some} =
       Fold.step1 (fn (x, IN (ac, pick)) =>
                      IN (pick (none, some)
                               (x, ac),
                          Pair.snd))
end
