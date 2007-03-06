(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Sq :> SQ = struct
   open Sq
   fun mk x = (x, x)
   fun map f (x, y) = (f x, f y)
end
