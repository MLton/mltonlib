(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic ARBITRARY
end

structure Generic : Generic = struct
   structure Open = WithArbitrary
     (open Generic
      structure HashRep = Open.Rep and TypeInfoRep = Open.Rep
      structure RandomGen = RanQD1Gen)
   open Generic Open
end
