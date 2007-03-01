(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Unit tests for the {SortedList} module.
 *)

val () = let
   open Type UnitTest

   local
      fun mk f = flip f Int.compare
      open SortedList
   in
      val insert     = mk insert
      val isSorted   = mk isSorted
      val merge      = mk merge
      val remove     = mk remove
      val stableSort = mk stableSort
   end

   val sortedList = let
      val l = list int
   in
      fn #? => withGen (RanQD1Gen.Monad.map (stableSort #?) (arbitrary l)) l
   end

   fun revPartition3Way c = let
      fun lp (ls, es, gs) =
          fn [] => (ls, es, gs)
           | x::xs =>
             lp (case c x of
                    LESS    => (x::ls, es, gs)
                  | EQUAL   => (ls, x::es, gs)
                  | GREATER => (ls, es, x::gs))
                xs
   in lp ([], [], [])
   end

   fun quickSort cmp = let
      fun lp sorted =
          fn p::xs =>
             let val (ls, es, gs) = revPartition3Way (cmp /> p) xs
             in lp (p::es @ lp sorted gs) ls
             end
           | [] => sorted
   in lp []
   end

   fun divide xs = let
      fun lp (gs, xs) x =
          fn (y::ys) =>
             lp (if x = y then
                    (gs, x::xs)
                 else
                    ((x, xs)::gs, []))
                y ys
           | [] => rev ((x, xs)::gs)
   in
      case quickSort Int.compare xs of
         [] => []
       | x::xs => lp ([], []) x xs
   end
in
   unitTests
      (title "SortedList")

      (chk (all (sortedList #n &` int)
                (fn xs & x => let
                       val ys = insert #n x xs
                    in
                       that (isSorted #n ys andalso
                             length ys = length xs + 1)
                    end)))

      (chk (all (sq (sortedList #n))
                (fn (xs, ys) =>  let
                       val zs = merge #n (xs, ys)
                    in
                       that (isSorted #n zs andalso
                             divide zs = divide (xs @ ys))
                    end)))

      (chk (all (list int)
                (fn xs => let
                       val ys = stableSort #n xs
                    in
                       that (isSorted #n ys andalso
                             divide xs = divide ys)
                    end)))

      (chk (all (list int)
                (fn xs => let
                       val ys = stableSort #1 xs
                    in
                       that (isSorted #1 ys andalso
                             map #1 (divide xs) = ys)
                    end)))

      $
end
