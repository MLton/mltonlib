(* Copyright (C) 2006 Entain, Inc.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
   
signature SOURCE_POS_STRUCTS = 
   sig
   end

signature SOURCE_POS = 
   sig
      include SOURCE_POS_STRUCTS

      type t

      val bogus: t
      val column: t -> int
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val file: t -> File.t
      val isBasis: t -> bool
      val line: t -> int
      val make: {column: int,
                 file: File.t,
                 line: int} -> t
      val posToString: t -> string
      val toString: t -> string
   end
