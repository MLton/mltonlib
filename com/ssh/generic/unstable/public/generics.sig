(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
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
   end

   structure Con : sig
      eqtype t
      val toString : t -> String.t
   end

   structure Record : sig
      type t
   end

   structure Tuple : sig
      type t
   end

   val L : String.t -> Label.t
   val C : String.t -> Con.t
end
