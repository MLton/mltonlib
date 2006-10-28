(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {TEXT} modules for MLKit == *)

structure Text       : TEXT        = MkTextExt (Text)
structure Char       : CHAR        = Text.Char
structure CharArray  : MONO_ARRAY  = Text.CharArray
structure CharVector : MONO_VECTOR = Text.CharVector
structure String     : STRING      = Text.String
