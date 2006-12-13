(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Process: PROCESS = struct

   structure Status = struct
      open OS.Process

      type t = status
   end

   open OS.Process

   val getEnv = Option.ofBasis o getEnv

end
