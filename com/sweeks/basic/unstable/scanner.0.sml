(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Scanner = struct

   datatype 'a t = T of Char.t Seq.t -> ('a * Char.t Seq.t) Option.t

   val make = T

   fun scan (T s, cs) = s cs

end
