(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Word      = MkWordSane (Word)
structure LargeWord = MkWordSane (LargeWord)
structure SysWord   = MkWordSane (SysWord)

structure Word8  = MkWordSane (Word8)

structure Word31 = MkWordSane (Word31)
structure Word32 = MkWordSane (Word32)

structure Word64 = MkWordSane (Word64)
