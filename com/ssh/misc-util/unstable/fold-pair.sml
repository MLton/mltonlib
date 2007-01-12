(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Utility module for pairing folds (see fold.sml).
 *)

(* XXX create FoldProduct for tupling an arbitrary number of folds easily *)

structure FoldPair = struct
   type ('a, 'b, 'c, 'd, 'e, 'f) t =
        ('a * 'b, 'c * 'd, 'e, 'f) Fold.t
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g) step0 =
        ('a * 'c, 'b * 'd, 'e, 'f, 'g) Fold.step0
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) step1 =
        ('a, 'b * 'd, 'c * 'e, 'f, 'g, 'h) Fold.step1
end

signature FOLD_PAIR = sig
   type ('a, 'b, 'c, 'd, 'e, 'f) t =
        ('a, 'b, 'c, 'd, 'e, 'f) FoldPair.t
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g) step0 =
        ('a, 'b, 'c, 'd, 'e, 'f, 'g) FoldPair.step0
   type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) step1 =
        ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) FoldPair.step1

   val fold : ('a, 'b, 'c, 'a * ('b -> 'c)) Fold.t
              * ('d, 'e, 'f, 'd * ('e -> 'f)) Fold.t
              -> ('c * 'f -> 'g)
              -> ('a, 'd, 'b, 'e, 'g, 'h) t
   val step0 : ('a, 'b, 'b, 'b, 'b) Fold.step0
               * ('c, 'd, 'd, 'd, 'd) Fold.step0
               -> ('a, 'b, 'c, 'd, 'e, 'f, 'g) step0
   val step1 : ('a, 'b, 'c, 'c, 'c, 'c) Fold.step1
               * ('a, 'd, 'e, 'e, 'e, 'e) Fold.step1
               -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) step1
end

structure FoldPair :> FOLD_PAIR = struct
   open FoldPair

   fun fold (l, r) f = let
      val (la, lf) = Fold.unfold l
      val (ra, rf) = Fold.unfold r
   in
      Fold.fold ((la, ra), f o Pair.map (lf, rf))
   end

   fun step0 (l, r) =
       Fold.step0 (Pair.map (Fold.unstep0 l,
                             Fold.unstep0 r))

   fun step1 (l, r) =
       Fold.step1 (Pair.map (Fold.unstep1 l,
                             Fold.unstep1 r)
                   o (fn (a11, (a12l, a12r)) =>
                         ((a11, a12l),
                          (a11, a12r))))
end
