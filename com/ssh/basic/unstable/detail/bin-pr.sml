(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BinPr :> BIN_PR = struct
   type 'a t = 'a Sq.t UnPr.t
end
