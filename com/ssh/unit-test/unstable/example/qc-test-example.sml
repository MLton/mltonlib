(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This file contains simple examples of specifying QuickCheck -style
 * randomized tests using the UnitTest framework.  The example laws
 * are from the QuickCheck paper by Koen Claessen and John Hughes.
 *)

(*
 * Note that a top-level module declaration is only required due to
 * the limitations of SML/NJ's CM and is not necessary with MLTon.
 * Specifically, the line
 *
 *> structure QCTestExample : sig end = struct
 *
 * could be replaced by a simple
 *
 *> let
 *
 * and the line
 *
 *> val () = unitTests
 *
 * by
 *
 *> in unitTests
 *
 * Also note that opening the {TopLevel} module and duplication of
 * fixity declarations is only required due to the limitations of
 * SML/NJ's CM.
 *)

structure QCTestExample : sig end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix & &`
   infixr |<
   (* SML/NJ workaround --> *)

   open Generic UnitTest

   local
      open SortedList
      (* The functions in the SortedList module are parameterized on both
       * a duplicate cardinality (either #1 or #n duplicates are allowed
       * and produced) and an ordering (a compare function).
       *)
   in
      val insert     = insert     #n Int.compare
      val isSorted   = isSorted   #n Int.compare
      val stableSort = stableSort #n Int.compare
   end

   val sortedList = let
      val l = list int
   in
      withGen (RandomGen.Monad.map stableSort (arbitrary l)) l
   end

   (* Note that one can (of course) make local auxiliary definitions, like
    * here, to help with testing.
    *)

   val () = unitTests
      (title "Reverse")

      (chk (all int
                (fn x =>
                    that (rev [x] = [x]))))

      (* Read the above as:
       *
       *   "check for all integers x that the reverse of the singleton
       *    list x equals the singleton list x"
       *
       * (Of course, in reality, the property is only checked for a small
       * finite number of random integers at a time.)
       *
       * In contrast to QuickCheck/Haskell, one must explicitly lift
       * boolean values to properties using {that}.
       *)

      (chk (all (sq (list int))
                (fn (xs, ys) =>
                    that (rev (xs @ ys) = rev ys @ rev xs))))

      (chk (all (list int)
                (fn xs =>
                    that (rev (rev xs) = xs))))

      (title "Functions")

      let
         infix ===
         fun (f === g) x = that (f x = g x)
         (* An approximation of extensional equality for functions. *)
      in
         chk (all (uop int &` uop int &` uop int)
                  (fn f & g & h =>
                      all int
                          (f o (g o h) === (f o g) o h)))

         (* Note that one can (of course) also write local auxiliary
          * definitions inside let -expressions.
          *)
      end

      (title "Conditional laws")

      (chk (all (sq int)
                (fn (x, y) =>
                    if x <= y then
                       that (Int.max (x, y) = y)
                    else
                       skip)))

      (* Read the above as:
       *
       *   "check for all integer pairs (x, y) that
       *    if x <= y then max (x, y) = y"
       *
       * In contrast to QuickCheck/Haskell, conditional properties are
       * specified using conditionals and {skip} rather than using an
       * implication operator.
       *)

      (title "Monitoring test data")

      (chk (all (int &` list int)
                (fn x & xs =>
                    if isSorted xs then
                       (trivial (null xs))
                          (that (isSorted (insert x xs)))
                    else
                       skip)))

      (chk (all (int &` list int)
                (fn x & xs =>
                    if isSorted xs then
                       (collect int (length xs))
                          (that (isSorted (insert x xs)))
                    else
                       skip)))

      (chk (all (int &` sortedList)
                (fn x & xs =>
                    that o isSorted |< insert x xs)))

      (* Above we use a custom test data generator for sorted (or ordered)
       * lists.  In contrast to QuickCheck/Haskell, the custom data
       * generator needs to be injected into a type-index (recall the use
       * of {withGen} in the implementation of sortedList above).
       *)

      $
end
