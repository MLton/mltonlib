(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Counter = struct
   exception Int of Int.t

   exception Incr
   exception Value of Actor.t
   exception Lock of Actor.t
   exception Unlock of Int.t

   fun new () = let
      open Cvt Actor
   in
      new (fn _ =>
         recur 0 (fn loop =>
            fn value =>
               (printlns ["Value: ", D value]
              ; receive (fn
                   Incr    => loop (value + 1)
                 | Value a => (a += Int value ; loop value)
                 | Lock a  => (a += Int value
                             ; receive (fn Unlock v => loop v))
                 | _       => loop value))))
   end
end
