(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Stream: STREAM =
struct

datatype 'a t = T of unit -> ('a t * 'a) option

fun get (T f) = f ()

val delay = T

fun delay' f = delay (get o f)

val empty = T (fn () => NONE)

fun prefix (s, x) = delay (fn _ => SOME (s, x))

fun singleton x = prefix (empty, x)

fun fold (s, b, f) =
   let
      fun loop (s, b) =
         case get s of
            NONE => b
          | SOME (s, x) => loop (s, f (x, b))
   in
      loop (s, b)
   end

fun toList s = rev (fold (s, [], op ::))

fun layout l s = List.layout l (toList s)
   
fun length s = fold (s, 0: int, fn (_, n) => n + 1)
   
fun memo f = delay (Promise.lazy f)

fun exists (s, f) =
   let
      fun loop s = Option.exists (get s, fn (s, x) => f x orelse loop s)
   in
      loop s
   end

fun foreach (s, f: 'a -> unit) =
   let
      fun loop s = Option.foreach (get s, fn (s, x) => (f x; loop s))
   in
      loop s
   end

fun isEmpty s = Option.isNone (get s)

fun map (s, f) =
   let
      fun loop s =
         delay (fn () => Option.map (get s, fn (s, x) => (loop s, f x)))
   in
      loop s
   end

fun append (s, s') =
   delay
   (fn () =>
    case get s of
       NONE => get s'
     | SOME (s, x) => SOME (append (s, s'), x))

fun appends ss =
   delay
   (fn () =>
    case get ss of
       NONE => NONE
     | SOME (ss, s) => get (append (s, appends ss)))

fun firstN (s, n: int) =
   delay'
   (fn () => 
    if n = 0
       then empty
    else (case get s of
             NONE => empty
           | SOME (s, x) => prefix (firstN (s, n - 1), x)))

fun equals (s, s', f) =
   let
      fun loop (s, s') =
         case (get s, get s') of
            (NONE, NONE) => true
          | (SOME (s, a), SOME (s', a')) => f (a, a') andalso loop (s, s')
          | _ => false
   in
      loop (s, s')
   end

fun peek2 (s, s', f) =
   let
      fun loop (s, s') =
         case (get s, get s') of
            (NONE, NONE) => NONE
          | (SOME (s, a), SOME (s', a')) =>
               if f (a, a') then SOME (a, a') else loop (s, s')
          | _ => Error.bug "Stream.peek2"
   in
      loop (s, s')
   end

end
