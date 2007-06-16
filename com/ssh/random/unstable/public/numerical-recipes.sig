(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for some utilities from Numerical Recipes in C.
 *)
signature NUMERICAL_RECIPES = sig
   val ranqd1 : Word32.t UnOp.t
   (**
    * This implements the quick-and-dirty linear congruential pseudo
    * random number generator described on page 284 of the book Numerical
    * Recipes in C.  Perhaps the most important feature of this generator
    * is that it cycles through all 32-bit words.  This is useful if you
    * want to generate unique 32-bit identifiers.
    *
    * Warning: If you need a high-quality pseudo random number generator
    * for simulation purposes, then this isn't for you.
    *)

   val psdes : Word32.t Sq.t UnOp.t
   (**
    * This implements the "Pseudo-DES" algorithm described in section 7.5
    * of the book Numerical Recipes in C.
    *)
end
