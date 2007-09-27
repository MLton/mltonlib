(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* This file contains simple examples of specifying QuickCheck -style
 * randomized tests using the UnitTest framework.  The example laws
 * are from the QuickCheck paper by Koen Claessen and John Hughes.
 *)

val () = let
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
in
   unitTests
      (title "Reverse")

      (testAll int
               (fn x =>
                   that (rev [x] = [x])))

      (* Read the above as:
       *
       *   "test for all integers x that the reverse of the singleton
       *    list x equals the singleton list x"
       *
       * Of course, in reality, the property is only checked for a small
       * finite number of random integers at a time.
       *
       * In contrast to QuickCheck, properties are explicitly checked
       * using {that} and other assertion procedures.
       *)

      (testAll (sq (list int))
               (fn (xs, ys) =>
                   that (rev (xs @ ys) = rev ys @ rev xs)))

      (testAll (list int)
               (fn xs =>
                   that (rev (rev xs) = xs)))

      (title "Functions")

      let
         infix ===
         fun (f === g) x = that (f x = g x)
         (* An approximation of extensional equality for functions. *)
      in
         testAll (case unOp int of t => t &` t &` t)
                 (fn f & g & h =>
                     all int
                         (f o (g o h) === (f o g) o h))

         (* Note that one can (of course) also write local auxiliary
          * definitions inside let -expressions.
          *)
      end

      (title "Conditional laws")

      (testAll (sq int)
               (fn (x, y) =>
                   if x <= y
                   then that (Int.max (x, y) = y)
                   else skip ()))

      (* Read the above as:
       *
       *   "test for all integer pairs (x, y) that
       *    if x <= y then max (x, y) = y"
       *
       * In contrast to QuickCheck, conditional properties are specified
       * using conditionals and {skip ()} rather than using an implication
       * operator.
       *)

      (title "Monitoring test data")

      (test (fn () =>
          withFreq (fn tbl =>
          all (int &` list int)
              (fn x & xs =>
                  if isSorted xs
                  then (collect int tbl (length xs)
                      ; that (isSorted (insert x xs)))
                  else skip ()))))

      (* Above we collect the generated sorted lists and print a table of
       * the frequencies of their lengths using {withFreq} and {collect}.
       *
       * In contrast to QuickCheck, data collection is not bolted into the
       * test framework.
       *)

      (test (fn () =>
          withFreq (fn tbl =>
          all (int &` sortedList)
              (fn x & xs =>
                  (collect int tbl (length xs)
                 ; that (isSorted (insert x xs)))))))

      (* Above we use a custom test data generator for sorted (or ordered)
       * lists.
       *
       * In contrast to QuickCheck, the custom data generator is
       * explicitly injected into a type representation.  Recall the use
       * of {withGen} in the implementation of {sortedList} above.
       *)

      $
end
