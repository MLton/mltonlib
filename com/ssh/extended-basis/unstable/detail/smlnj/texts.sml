(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {TEXT} modules for SML/NJ == *)

structure Text       = MkTextExt (Text)
structure Char       = Text.Char
structure CharArray  = Text.CharArray
structure CharVector = Text.CharVector
structure String     = Text.String
