(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure PtrCache =
   MkIntObjCache
      (type t = C.voidptr
       local
          val unused = ref [] (* XXX free these at some point *)
       in
          fun set k = C.Ptr.|*! (C.Ptr.cast' k) <\ C.Set.sint'
          fun new value =
              case List.pop unused of
                 SOME v => v
               | NONE => let
                    val k = C.Ptr.inject' (C.Ptr.|&! (C.new' C.S.sint))
                 in
                    set k value ; k
                 end
          val discard = unused <\ List.push
          val get = C.Get.sint' o C.Ptr.|*! o C.Ptr.cast'
       end)
