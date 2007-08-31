(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure HashMap :> sig
   type ('a, 'b) t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, 'b) t
   val insert : ('a, 'b) t -> ('a * 'b) Effect.t
   val find : ('a, 'b) t -> 'a -> 'b Option.t
   val numItems : ('a, 'b) t -> Int.t
end = struct
   open HashTable
   type ('a, 'b) t = ('a, 'b) hash_table
   fun new {eq, hash} = mkTable (hash, eq) (127, Subscript)
end
