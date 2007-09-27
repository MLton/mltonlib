(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* This whole file is a SML/NJ workaround. *)

signature Generic = sig include Generic DATA_REC_INFO end
structure Generic : Generic = struct
   structure Open = WithDataRecInfo (Generic)
   open Generic Open
end

signature Generic = sig include Generic SOME end
structure Generic : Generic = struct
   structure Open = WithSome
     (open Generic
      structure TypeInfoRep = Open.Rep)
   open Generic Open
end

signature Generic = sig include Generic PICKLE end
structure Generic : Generic = struct
   structure Open = WithPickle
     (open Generic
      structure DataRecInfoRep = Open.Rep and EqRep = Open.Rep
            and HashRep = Open.Rep and SomeRep = Open.Rep
            and TypeHashRep = Open.Rep and TypeInfoRep = Open.Rep)
   open Generic Open
end

signature Generic = sig include Generic SEQ end
structure Generic : Generic = struct
   structure Open = WithSeq
     (open Generic
      structure HashRep = Open.Rep)
   open Generic Open
end

signature Generic = sig include Generic REDUCE end
structure Generic : Generic = struct
   structure Open = WithReduce (Generic)
   open Generic Open
end

signature Generic = sig include Generic TRANSFORM end
structure Generic : Generic = struct
   structure Open = WithTransform
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

local structure ? = RegBasisExns (Generic) open ? in end
