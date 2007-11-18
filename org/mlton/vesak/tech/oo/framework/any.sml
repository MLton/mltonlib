(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Any : ANY = struct
   type 'a t = 'a
   fun part _ = ()
   val getSub = id
   val mapSub = id
end
