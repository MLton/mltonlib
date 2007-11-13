(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature HASH_MAP = sig
   type ('a, 'b) t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, 'b) t
   val size : ('a, 'b) t -> Int.t
   val insert : ('a, 'b) t -> ('a * 'b) Effect.t
   val find : ('a, 'b) t -> 'a -> 'b Option.t
end
