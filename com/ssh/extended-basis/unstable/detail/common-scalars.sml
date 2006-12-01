(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Extended scalar modules common to all compilers *)

structure Int = MkIntegerExt (Int)
structure LargeInt = MkIntegerExt (LargeInt)
structure Position = MkIntegerExt (Position)

structure LargeReal = MkRealExt (LargeReal)
structure Real = MkRealExt (Real)

structure LargeWord = MkWordExt (LargeWord)
structure Word = MkWordExt (Word)
structure Word8 = MkWordExt (Word8)
