(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is a simple example of using iterator combinators.  Given a text
 * file, this program prints the file to standard output with line
 * numbers.
 *)

open Cvt Iter

val () =
    case CommandLine.arguments ()
     of [file] =>
        (index From 1 $ (inTextFile file By lines $))
         (fn ln & i => prints [D i, ":\t", ln])
      | _ =>
        printlns ["Usage: ", OS.Path.file (CommandLine.name ()), " <file>"]
