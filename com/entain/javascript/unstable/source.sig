(* Copyright (C) 2006 Entain, Inc.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SOURCE_STRUCTS =
   sig
   end

signature SOURCE =
   sig
      include SOURCE_STRUCTS
         
      type t

      (* The pos in the following specs is a file position, e.g. yypos of mllex.
       *)
      val getPos: t * int -> SourcePos.t
      val lineDirective:
         t * File.t option * {lineNum: int, lineStart: int} -> unit
      val lineStart: t -> SourcePos.t
      val new: File.t -> t
      val newline: t * int -> unit
   end
