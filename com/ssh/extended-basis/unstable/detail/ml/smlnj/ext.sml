(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ext = struct
   structure Exn = struct
      fun addMessager _ = ()
      val history = SMLofNJ.exnHistory
   end
end
