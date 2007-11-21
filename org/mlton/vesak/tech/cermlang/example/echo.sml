(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open CerMLang

(* This example is roughly a transliteration of "An Echo process" example
 * from [http://www.erlang.org/course/concurrent_programming.html#echo An
 * Erlang Course].
 *)

exception Echo of Proc.t * String.t
exception Stop

fun echo () =
    recv (fn Stop        => (fn () => ())
           | Echo (s, m) => (fn () => (s <- Echo (self (), m) ; echo ())))

val () = start (fn () => let
   val echo = spawn echo
in
   echo <- Echo (self (), "Hi!")
 ; recv (fn Echo (_, msg) => (fn () => println ("Echo says: "^msg)))
 ; echo <- Stop
end)
