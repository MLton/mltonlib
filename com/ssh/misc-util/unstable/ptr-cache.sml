(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   structure PtrIntObj = struct
      (* XXX Simplify *)
      type t = C.voidptr
      local
         val nUsed = ref 0
         val nUnused = ref 0
         val unused = ref C.Ptr.null'
         fun value k = U_PtrIntObj.f_value' (C.Ptr.|*! k)
         fun next k = U_PtrIntObj.f_next' (C.Ptr.|*! k)
         fun pop () = let
            val k = !unused
         in
            if C.Ptr.isNull' k
            then NONE
            else SOME k before (unused := C.Get.ptr' (next (!unused))
                              ; nUnused -= 1)
         end
      in
         fun set k v = C.Set.sint' (value (C.Ptr.cast' k), v)
         fun get k = C.Get.sint' (value (C.Ptr.cast' k))
         fun new v = let
            val k =
                C.Ptr.inject'
                   (case pop () of
                       SOME k => k
                     | NONE => C.Ptr.|&! (C.new' U_PtrIntObj.size))
         in
            nUsed += 1 ; set k v ; k
         end
         fun discard k = let
            val k = C.Ptr.cast' k
         in
            C.Set.ptr' (next k, !unused) ; unused := k
          ; nUnused += 1 ; nUsed -= 1
          ; while !nUsed < !nUnused do
               case pop () of
                  NONE => raise Fail "bug"
                | SOME k => C.free' k
         end
      end
   end
in
   (** A cache whose keys are C pointers. *)
   structure PtrCache = MkIntObjCache (PtrIntObj)
end
