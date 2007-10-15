(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic DATA_REC_INFO
end

functor MkGeneric (Arg : Generic) = struct
   structure Open = MkGeneric (Arg)
   open Arg Open
   structure DataRecInfoRep = Open.Rep
end

structure Generic =
   MkGeneric (structure Open = WithDataRecInfo (Generic)
              open Generic Open)
