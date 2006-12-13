(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature STATIC_SUM = sig
   
   type ('a1, 'a2, 'b1, 'b2, 'c) u
   type ('a1, 'a2, 'b1, 'b2, 'c) t = Unit.t -> ('a1, 'a2, 'b1, 'b2, 'c) u

   val left: 'a1 -> ('a1, 'a2, 'b1, 'b2, 'a2) t
   val right: 'b1 -> ('a1, 'a2, 'b1, 'b2, 'b2) t
   val switch: ('a1, 'a2, 'b1, 'b2, 'c) t * ('a1 -> 'a2) * ('b1 -> 'b2) -> 'c

end
