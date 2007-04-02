(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TextIO = struct
   open TextIO
   val getReader = StreamIO.getReader o getInstream
   val getWriter = StreamIO.getWriter o getOutstream
end
