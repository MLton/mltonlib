(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Vector : VECTOR = struct
   structure Common = MkSeqCommonExt (Vector)
   open Common Vector
   fun iso ? = Pair.map (map, map) ?
end
