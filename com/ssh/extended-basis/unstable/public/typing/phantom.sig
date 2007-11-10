(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Phantom} structure that defines some general purpose
 * Phantom types (types that have no values).
 *)
signature PHANTOM = sig
   type yes
   type no 
end
