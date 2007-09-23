(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig structure Open : OPEN_CASES end
structure Generic : Generic = struct
   structure Open = RootGeneric
end

signature Generic = sig include Generic EQ end
structure Generic : Generic = struct
   structure Open = WithEq (Generic)
   open Generic Open
end

signature Generic = sig include Generic TYPE_HASH end
structure Generic : Generic = struct
   structure Open = WithTypeHash (Generic)
   open Generic Open
end

signature Generic = sig include Generic TYPE_INFO end
structure Generic : Generic = struct
   structure Open = WithTypeInfo (Generic)
   open Generic Open
end

signature Generic = sig include Generic HASH end
structure Generic : Generic = struct
   structure Open = WithHash
     (open Generic
      structure TypeHashRep = Open.Rep and TypeInfoRep = Open.Rep)
   open Generic Open
end

signature Generic = sig include Generic ORD end
structure Generic = struct
   structure Open = WithOrd (Generic)
   open Generic Open
end

signature Generic = sig include Generic PRETTY end
structure Generic = struct
   structure Open = WithPretty
     (open Generic
      structure HashRep = Open.Rep)
   open Generic Open
end

structure Generic = struct
   structure Rep = ClosePrettyWithExtra
     (open Generic
      structure PrettyRep = Open.Rep)
   open Generic Rep
end
