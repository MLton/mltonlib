(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A functor for lifting a structural type-index to a type-index.
 *)

functor StructuralTypeToType
           (S : STRUCTURAL_TYPE) :>
TYPE
   where type 'a t = 'a S.t
   where type 'a s = 'a S.t
   where type ('a, 'k) p = 'a S.t = struct
   open S

   type 'a s = 'a t
   type ('a, 'k) p = 'a t

   val isoProduct = iso
   val isoSum     = iso

   val T = id
   fun R _ = id

   val tuple = id
   val record = id

   fun C0 _ = unit
   fun C1 _ = id

   val data = id
end
