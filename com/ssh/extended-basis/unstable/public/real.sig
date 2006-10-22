(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {REAL} signature.
 *)
signature REAL = sig
   include REAL

   (** == Embeddings == *)

   val embDecimal : (real, IEEEReal.decimal_approx) emb
   val embString : (real, string) emb

   (** == Isomorphisms == *)

   val isoInt : (real, Int.int) iso
   val isoLarge : (real, LargeReal.real) iso
   val isoLargeInt : (real, LargeInt.int) iso
   val isoManExp : (real, {man : real, exp : int}) iso
end
