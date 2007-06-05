(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for liftings of generic functions.
 *)
signature GENERIC_LIFTING = sig
   structure Element : GENERIC_INDEX
   (** The element of the combined type-index. *)

   structure Of : GENERIC_INDEX
   (** The combined type-index. *)

   val lifting : ('a Element.t, 'a Of.t) Lifting.t Thunk.t
   (**
    * The lifting index for lifting operations on values of the element
    * type to operations on the elements of the combined type.
    *)
end
