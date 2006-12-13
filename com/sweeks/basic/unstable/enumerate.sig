(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature ENUMERATE = sig
   type 'a const
   type 'a elem
   type 'a state
   type 'a t
   val start: 'a t -> 'a const * 'a state
   val next: 'a const * 'a state -> ('a elem * 'a state) Option.t
end
