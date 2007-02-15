(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure PtrIntObj = struct
      type t = C.voidptr
      local
         val nUsed = ref 0
         val nUnused = ref 0
         val unused = ref []
      in
         fun set k = C.Ptr.|*! (C.Ptr.cast' k) <\ C.Set.sint'
         fun new v = let
            val k = case List.pop unused of
                       SOME k => (nUnused -= 1 ; k)
                     | NONE => C.Ptr.inject' (C.Ptr.|&! (C.new' C.S.sint))
         in
            nUsed += 1 ; set k v ; k
         end
         fun discard k =
             (List.push (unused, k) ; nUnused += 1 ; nUsed -= 1
            ; while !nUsed < !nUnused do
                 case List.pop unused of
                    NONE => raise Fail "bug"
                  | SOME k => (nUnused -= 1 ; C.free' k))
         val get = C.Get.sint' o C.Ptr.|*! o C.Ptr.cast'
      end
   end
in
   (** A cache whose keys are C pointers. *)
   structure PtrCache = MkIntObjCache (PtrIntObj)
end
