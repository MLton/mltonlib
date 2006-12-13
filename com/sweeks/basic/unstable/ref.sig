(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature REF = sig

   datatype t = datatype Ref.t
   (**
    * SML's reference type.
    *)

   val ! : 'a t -> 'a
   (**
    * !r returns the value stored in r.
    *)
   val := : 'a t * 'a -> Unit.t
   (**
    * r := v changes the value in r to v.
    *)

end
