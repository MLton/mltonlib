(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {ArraySlice :> ARRAY_SLICE} structure.
 *)
structure ArraySlice : ARRAY_SLICE = struct
   open ArraySlice
   type 'a t = 'a slice
end
