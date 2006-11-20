(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Ext = struct
   structure Exn = struct
      local
         open MLton.Exn
      in
         val addMessager = addExnMessager
         val history = history
      end
   end
end
