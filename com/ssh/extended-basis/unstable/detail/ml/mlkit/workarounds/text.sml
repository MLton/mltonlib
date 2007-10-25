(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Text = struct
   open Text
   structure String = struct
      val scan = fn _ => raise Fail "SML/NJ does not implement String.scan"
      open String
   end
end
