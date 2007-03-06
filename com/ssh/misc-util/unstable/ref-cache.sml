(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure RefIntObj : INT_OBJ = struct
      type t = Int.t Ref.t
      val new = ref
      val discard = ignore
      val get = !
      fun set r = r <\ op :=
   end
in
   (** A cache whose keys are {int ref} cells. *)
   structure RefCache = MkIntObjCache (RefIntObj)
end
