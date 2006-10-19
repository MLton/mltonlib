(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {REAL} signature.
 *)

signature REAL =
   sig
      include REAL
      val decimalEmb : (real, IEEEReal.decimal_approx) emb
      val intIso : (real, Int.int) iso
      val largeIso : (real, LargeReal.real) iso
      val largeIntIso : (real, LargeInt.int) iso
      val manExpIso : (real, {man : real, exp : int}) iso
      val stringEmb : (real, string) emb
   end
