(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure SysError: SYS_ERROR = struct

   open OS

   structure Exn = struct
      type error = syserror
      type t = String.t * error Basis.Option.t

      exception E = OS.SysErr

      val message = fst
      val error = Option.ofBasis o snd
   end

   type t = Exn.error

   val == = op =
   val message = errorMsg
   val name = errorName
   val ofName = Option.ofBasis o syserror

end
