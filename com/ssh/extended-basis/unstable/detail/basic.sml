(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Basic :> BASIC = struct
   fun eq x y = x = y
   fun notEq x y = x <> y
   fun fail m = raise Fail m
   fun failing m _ = fail m
   fun raising e _ = raise e
   fun recur x = Fn.flip Fn.fix x
   fun repeat f n x = if n = 0 then x else repeat f (n-1) (f x)
   fun undefined _ = fail "undefined"
end
