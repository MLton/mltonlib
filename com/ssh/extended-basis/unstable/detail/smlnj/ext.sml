(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Ext = struct
   structure Exn = struct
      fun addMessager _ = ()
      val history = SMLofNJ.exnHistory
   end
end
