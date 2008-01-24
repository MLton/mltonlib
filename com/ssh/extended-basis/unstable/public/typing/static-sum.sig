(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A static sum allows one to make choices at the type level.
 *
 * As an example, consider the following function:
 *
 *> fun succ x =
 *>     match x (fn i => i + 1,
 *>              fn r => r + 1.0)
 *
 * Now,
 *
 *> succ (inL 2) = 3
 *> succ (inR 1.5) = 2.5
 *
 * In other words, {succ} is a function that is given a static sum that
 * holds either an int or a real.  {succ} then returns the value plus 1.
 *
 * The design is mostly copied from Stephen Weeks.
 *)
signature STATIC_SUM = sig
   type ('dL, 'cL, 'dR, 'cR, 'c) dom
   type ('dL, 'cL, 'dR, 'cR, 'c) cod
   type ('dL, 'cL, 'dR, 'cR, 'c) t =
        ('dL, 'cL, 'dR, 'cR, 'c) dom -> ('dL, 'cL, 'dR, 'cR, 'c) cod
   (** The type of static sums. *)

   val inL : 'a -> ('a, 'b, 'c, 'd, 'b) t
   (** Injects the given value to a static sum as the left element. *)

   val inR : 'c -> ('a, 'b, 'c, 'd, 'd) t
   (** Injects the given value to a static sum as the right element. *)

   val match : ('a, 'b, 'c, 'd, 'e) t -> ('a -> 'b) * ('c -> 'd) -> 'e
   (**
    * Performs case analysis on the given static sum.  {match} satisfies
    * the following laws:
    *
    *> match (inL x) (f, g) = f x
    *> match (inR x) (f, g) = g x
    *)

   val split : ('a,
                ('a, 'b, 'c, 'd, 'b) t * ('a, 'e, 'f, 'g, 'e) t, 'h,
                ('i, 'j, 'h, 'k, 'k) t * ('l, 'm, 'h, 'n, 'n) t, 'o) t -> 'o
   (**
    * Splits a given static sum into two "branches" that can be assigned
    * types independently.  {split} satisfies the following laws:
    *
    *> split (inL x) = (inL x, inL x)
    *> split (inR x) = (inR x, inR x)
    *
    * {split} is not primitive, it can be implemented as:
    *
    *> fun split x = match x (fn x => (inL x, inL x),
    *>                        fn x => (inR x, inR x))
    *)

   val out : ('a, 'a, 'b, 'b, 'c) t -> 'c
   (**
    * Extracts the value from the given static sum.  {out} satisfies the
    * following laws:
    *
    *> out (inL x) = x
    *> out (inR x) = x
    *
    * {out} is not primitive, it can be implemented as:
    *
    *> fun out s = match s (id, id)
    *)
end
