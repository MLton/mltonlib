(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {WORD} modules for SML/NJ == *)

structure Word      = MkWordExt (Word)
structure LargeWord = MkWordExt (LargeWord)
structure SysWord   = MkWordExt (SysWord)

structure Word8  = MkWordExt (Word8)

structure Word31 = MkWordExt (Word31)
structure Word32 = MkWordExt (Word32)

structure Word64 = MkWordExt (Word64)
