(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   open Actor

   val actor =
       new (fn this => let
                  val counter = Counter.new ()
               in
                  start counter
                ; counter += Counter.Incr
                ; counter += Counter.Value this
                ; receive (fn Counter.Int v =>
                              println (Int.toString v))
               end)
in
   start actor
 ; Async.Handler.runAll ()
end
