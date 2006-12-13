(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature SEQ = sig

   type 'a t

   include SEQUENCE where type 'a t0 = 'a t

   val delay: (Unit.t -> 'a t) -> 'a t
   (**
    * delay f returns a sequence that is like f(), except that f() is not
    * computed until the sequence is needed.
    *)
   val get: 'a t -> ('a * 'a t) Option.t
   (**
    * get [] = None
    * get [x0, x1, ..., xn-1] = (x0, [x1, ..., xn-1])
    *)

end
