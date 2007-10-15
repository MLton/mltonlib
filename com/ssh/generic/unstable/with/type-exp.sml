(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic TYPE_EXP
end

functor MkGeneric (Arg : Generic) = struct
   structure Open = MkGeneric (Arg)
   open Arg Open
   structure TypeExpRep = Open.Rep
end

structure Generic =
   MkGeneric (structure Open = WithTypeExp (Generic)
              open Generic Open)
