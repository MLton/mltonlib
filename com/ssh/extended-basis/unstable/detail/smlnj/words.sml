(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {WORD} modules for SML/NJ == *)

structure SysWord = MkWordExt (SysWord)

structure Word31 = MkWordExt (Word31)
structure Word32 = MkWordExt (Word32)

structure Word64 = MkWordExt (Word64)
