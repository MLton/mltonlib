(* Copyright (C) 2006 Entain, Inc.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature TWO_POINT_LATTICE_STRUCTS = 
   sig
      (* pretty print names *)
      val bottom: string
      val top: string
   end

signature TWO_POINT_LATTICE = 
   sig
      include TWO_POINT_LATTICE_STRUCTS
      
      type t

      val <= : t * t -> unit (* force rhs to be top if lhs is *)
      val == : t * t -> unit (* force lhs and rhs to be the same *)
      (* handler will be run once iff value becomes top *)
      val addHandler: t * (unit -> unit) -> unit
      val isBottom: t -> bool
      val isTop: t -> bool
      val layout: t -> Layout.t
      val makeTop: t -> unit
      val new: unit -> t (* a new bottom *)
   end
