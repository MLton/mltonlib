(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended modules common to all compilers == *)

structure Int = MkIntegerExt (Int)
structure LargeInt = MkIntegerExt (LargeInt)
structure Position = MkIntegerExt (Position)

structure LargeReal = MkRealExt (LargeReal)
structure Real = MkRealExt (Real)

structure LargeWord = MkWordExt (LargeWord)
structure Word = MkWordExt (Word)
structure Word8 = MkWordExt (Word8)

structure Word8Vector = MkMonoVectorExt (Word8Vector)
structure Word8Array = MkMonoArrayExt (structure MonoArray = Word8Array
                                       structure MonoVector = Word8Vector)

structure Text = MkTextExt (Text)
structure Char = Text.Char
structure CharArray = Text.CharArray
structure CharVector = Text.CharVector
structure String = Text.String
structure Substring = Text.Substring
