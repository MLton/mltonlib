(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Buffer :> BUFFER = struct
   structure Buffer =
      MkBufferCommon (type 'a elem = 'a
                      val inj = Fn.id val prj = Fn.id val any = Fn.id)
   open Buffer

   fun reserve b newCap =
       if newCap <= capacity b orelse isEmpty b then ()
       else realloc (asub (array b) 0) b newCap
end
