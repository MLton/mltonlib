(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A static sum allows one to write functions with argument type dependent
 * result types.
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
 * The type of the result of {succ} depends on the type of the argument.
 *
 * Here is another example:
 *
 *> fun plus (a, b) =
 *>     (match b o match a)
 *>        (fn a => (fn b => inL (Int.+  (     a,      b)),
 *>                  fn b => inR (Real.+ (real a,      b))),
 *>         fn a => (fn b => inR (Real.+ (     a, real b)),
 *>                  fn b => inR (Real.+ (     a,      b))))
 *
 * Try to figure out how it should be called and what does it do.
 *
 * The design is mostly copied from Stephen Weeks.
 *
 * See also: [http://mlton.org/StaticSum]
 *)
signature STATIC_SUM = sig
   type ('dL, 'cL, 'dR, 'cR, 'c) dom
   type ('dL, 'cL, 'dR, 'cR, 'c) cod
   type ('dL, 'cL, 'dR, 'cR, 'c) t =
        ('dL, 'cL, 'dR, 'cR, 'c) dom -> ('dL, 'cL, 'dR, 'cR, 'c) cod
   (**
    * The type of static sums.  In the type variables, {'d} stands for
    * domain and {'c} for codomain.
    *
    * The key difference between an ordinary sum type, like {(int, real)
    * Sum.t}, and a static sum type, like {(int, real, real, int, real)
    * StaticSum.t}, is that the ordinary sum type says nothing about the
    * type of the result of deconstructing a sum while the static sum type
    * specifies the type.
    *)

   val inL : 'dL -> ('dL, 'cL, 'dR, 'cR, 'cL) t
   (** Injects the given value to a static sum as the left element. *)

   val inR : 'dR -> ('dL, 'cL, 'dR, 'cR, 'cR) t
   (** Injects the given value to a static sum as the right element. *)

   val map : ('dL0 -> 'dLL) * ('dR0 -> 'dRR) ->
             ('dL0, ('dLL, 'cLL, 'dRL, 'cRL, 'cLL) t,
              'dR0, ('dLR, 'cLR, 'dRR, 'cRR, 'cRR) t,
              ('dL, 'cL, 'dR, 'cR, 'c) t) t -> ('dL, 'cL, 'dR, 'cR, 'c) t
   (**
    * Applies one of the given functions to the value carried by the
    * static sum.  {map} satisfies the following laws:
    *
    *> map (f, g) (inL x) = inL (f x)
    *> map (f, g) (inR x) = inR (g x)
    *
    * {map} is not primitive, it can be implemented as:
    *
    *> fun map (f, g) = sum (inL o f, inR o g)
    *)

   val match : ('dL, 'cL, 'dR, 'cR, 'c) t -> ('dL -> 'cL) * ('dR -> 'cR) -> 'c
   (**
    * Performs case analysis on the given static sum.  {match} satisfies
    * the following laws:
    *
    *> match (inL x) (f, g) = f x
    *> match (inR x) (f, g) = g x
    *)

   val out : ('L, 'L, 'R, 'R, 'c) t -> 'c
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

   val split :
       ('dL, ('dL,   'cLL1, 'dRL1, 'cRL1, 'cLL1) t *
             ('dL,   'cLL2, 'dRL2, 'cRL2, 'cLL2) t,
        'dR, ('dLR1, 'cLR1,   'dR, 'cRR1, 'cRR1) t *
             ('dLR2, 'cLR2,   'dR, 'cRR2, 'cRR2) t,
        ('dL1, 'cL1, 'dR1, 'cR1, 'c1) t * ('dL2, 'cL2, 'dR2, 'cR2, 'c2) t) t ->
       ('dL1, 'cL1, 'dR1, 'cR1, 'c1) t * ('dL2, 'cL2, 'dR2, 'cR2, 'c2) t
   (**
    * Splits a given static sum into two "branches" that can be assigned
    * types independently.  {split} satisfies the following laws:
    *
    *> split (inL x) = (inL x, inL x)
    *> split (inR x) = (inR x, inR x)
    *
    * {split} is not primitive, it can be implemented as:
    *
    *> fun split x =
    *>     match x (fn x => (inL x, inL x),
    *>              fn x => (inR x, inR x))
    *)

   val sum : ('dL -> 'cL) * ('dR -> 'cR) -> ('dL, 'cL, 'dR, 'cR, 'c) t -> 'c
   (** {sum} is equivalent to {flip match}. *)
end
