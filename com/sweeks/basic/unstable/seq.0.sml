(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Seq = struct

   local
      datatype 'a t = T of Unit.t -> ('a * 'a t) Option.t
   in
      type 'a t = 'a t

      fun get (T f) = f ()

      fun delay f = T (get o f)
         
      fun empty () = T (fn () => None)
         
      fun cons (x, s) = T (fn () => Some (x, s))
   end

end

