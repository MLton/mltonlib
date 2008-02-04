(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for supporting primitives required by generics.
 *)
signature GENERICS = sig
   structure Label : sig
      eqtype t
      val toString : t -> String.t
      val hash : t -> Word32.t
   end

   structure Con : sig
      eqtype t
      val toString : t -> String.t
      val hash : t -> Word32.t
   end

   structure Record : T
   structure Tuple : T

   val L : String.t -> Label.t
   val C : String.t -> Con.t
end
