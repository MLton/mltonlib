(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature GET = sig
   type 'a elem
   type 'a t
   val get: 'a t -> ('a elem * 'a t) Option.t
end
