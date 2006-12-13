(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature BOOL = sig

   datatype t = datatype Bool.t

   val not: t -> t
   (**
    * not true = false.  not false = true.
    *)     

end
