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

   val decimalEmb : (real, IEEEReal.decimal_approx) emb
   val stringEmb : (real, string) emb

   (** == Isomorphisms == *)

   val intIso : (real, Int.int) iso
   val largeIntIso : (real, LargeInt.int) iso
   val largeIso : (real, LargeReal.real) iso
   val manExpIso : (real, {man : real, exp : int}) iso
end
