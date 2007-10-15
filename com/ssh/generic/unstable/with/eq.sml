(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic EQ
end

functor MkGeneric (Arg : Generic) = struct
   structure Open = MkGeneric (Arg)
   open Arg Open
   structure EqRep = Open.Rep
end

structure Generic =
   MkGeneric (structure Open = WithEq (Generic)
              open Generic Open)
