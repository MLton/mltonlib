(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for the "rep" of generics.
 *)
signature REP = sig
   structure Open : sig
      structure Rep : OPEN_REP
   end
end
