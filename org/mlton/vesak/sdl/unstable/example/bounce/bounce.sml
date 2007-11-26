(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

fun say ms = println (concat ms)

fun main () =
    ()

val () =
    (SDL.init SDL.Init.EVERYTHING
   ; say ["SDL initialized"]
   ; after (main, SDL.quit))
