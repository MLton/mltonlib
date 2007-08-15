(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Extended scalar modules common to all compilers *)

structure FixedInt = MkIntegerExt (FixedInt)
structure Int = MkIntegerExt (BasisInt)
structure LargeInt = MkIntegerExt (BasisLargeInt)
structure Position = MkIntegerExt (BasisPosition)

structure LargeReal = MkRealExt (BasisLargeReal)
structure Real = MkRealExt (BasisReal)

structure LargeWord = MkWordExt (BasisLargeWord)
structure Word = MkWordExt (BasisWord)
structure Word8 = MkWordExt (BasisWord8)
