(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is based on Haskell code from the thread
 *
 *   Pancake sorting with the shortest series of flips, a la brute force,
 *   from new Haskeller.
 *   http://groups.google.com/group/comp.lang.haskell/browse_frm/thread/9151e2be8aef1cc4#
 *
 * on comp.lang.haskell.  Of course, this version uses iterators in SML
 * rather than lazy lists in Haskell.
 *)

open Cvt Iter

val filter = Monad.filter

val rec isSorted =
 fn []      => true
  | [_]     => true
  | f::s::r => f <= s andalso isSorted (s::r)

fun reverseTop (n, s) = List.revAppend (List.split (s, n))

fun variations m n = let
   fun vars y =
    fn 0 => return []
     | n => filter (notEq y) (upTo (m+1) From 2 $) >>= (fn x =>
            vars x (n-1) >>= (fn xs =>
            return (x::xs)))
in
   vars 0 n
end

fun incVariations m = up From 1 $ >>= variations m

fun exec ? = foldl reverseTop ?

fun search xs =
    if isSorted xs
    then SOME []
    else first (filter (isSorted o exec xs) (incVariations (length xs)))

val xs = List.mapPartial Int.fromString (CommandLine.arguments ())

val () =
    case search xs
     of NONE    => println "Impossible!"
      | SOME is =>
        recur (is & xs) (fn lp =>
           fn []    &  _ => ()
            | i::is & xs =>
              case reverseTop (i, xs)
               of ys =>
                  (printlns ["reverseTop (", D i, ", ", L D xs, ") => ", L D ys]
                 ; lp (is & ys)))
