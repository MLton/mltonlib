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
      val decimal : (real, IEEEReal.decimal_approx) emb
      val int : (real, Int.int) iso
      val large : (real, LargeReal.real) iso
      val largeInt : (real, LargeInt.int) iso
      val manExp : (real, {man : real, exp : int}) iso
      val string : (real, string) emb
   end
