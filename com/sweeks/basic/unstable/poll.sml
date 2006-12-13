(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Poll: POLL = struct

   open OS.IO

   fun poll (ds, t) = OS.IO.poll (ds, Option.toBasis t)

   structure Desc = struct
      datatype t = datatype poll_desc

      exception Poll = OS.IO.Poll

      val addIn = pollIn
      val addOut = pollOut
      val addPri = pollPri
      val ofIO = Option.ofBasis o pollDesc
      val toIO = pollToIODesc
   end

   structure Info = struct
      type t = poll_info

      val isIn = isIn
      val isOut = isOut
      val isPri = isPri
      val toDesc = infoToPollDesc
   end

end
