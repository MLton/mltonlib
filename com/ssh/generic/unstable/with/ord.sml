(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic ORD
end

structure Generic : Generic = struct
   structure Open = WithOrd
     (open Generic
      structure HashRep = Open.Rep)
   open Generic Open
end
