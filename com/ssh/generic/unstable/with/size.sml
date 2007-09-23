(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic SIZE
end

structure Generic : Generic = struct
   structure Open = WithSize
     (open Generic
      structure HashRep = Open.Rep and TypeInfoRep = Open.Rep)
   open Generic Open
end
