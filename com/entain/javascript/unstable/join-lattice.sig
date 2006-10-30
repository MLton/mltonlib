(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature JOIN_LATTICE_STRUCTS = 
   sig
      structure Element:
         sig
            type t
               
            val join: t * t -> t
            val layout: t -> Layout.t
            val <= : t * t -> bool
         end
   end

signature JOIN_LATTICE = 
   sig
      include JOIN_LATTICE_STRUCTS
      
      type t

      val <= : t * t -> unit
      val ensureAtLeast: t * Element.t -> unit
      val layout: t -> Layout.t
      val new: Element.t -> t
      val value: t -> Element.t
   end
