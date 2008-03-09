(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeHash (Arg : WITH_TYPE_HASH_DOM) : TYPE_HASH_CASES = struct
   structure W = Word32

   fun unary c h : W.t = h * 0w19 + c
   fun binary c (l, r) : W.t = l * 0w13 + r * 0w17 + c

   structure TypeHashRep = LayerRep' (open Arg type 'a t = W.t)

   val typeHash = TypeHashRep.This.getT

   structure Open = LayerCases
     (fun iso        ? _ = unary 0wxD00B6B6B ?
      fun isoProduct ? _ = unary 0wxC01B56DB ?
      fun isoSum     ? _ = unary 0wxB006B6DB ?

      val op *`  = binary 0wx00ADB6DB
      val T      = unary 0wx00B6DB6B
      fun R    l = unary (Generics.Label.hash l)
      val tuple  = unary 0wx00DB6DB5
      val record = unary 0wx01B6DB55

      val op +` = binary 0wx02DB6D4D
      val unit  = 0wx036DB6C5 : W.t
      val C0    = Generics.Con.hash
      fun C1  c = unary (Generics.Con.hash c)
      val data  = unary 0wx04DB6D63

      val Y = Tie.id (0wx05B6DB51 : W.t)

      val op --> = binary 0wx06DB6D61

      val exn = 0wx08DB6B69 : W.t
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      val list = unary 0wx09B6DB29

      val vector = unary 0wx0ADB6D29

      val array = unary 0wx0B6DB651
      val refc = unary 0wx0CDB6D51

      val fixedInt = 0wx0DB6DAA1 : W.t
      val largeInt = 0wx1B6DB541 : W.t

      val largeReal = 0wx2DB6D851 : W.t
      val largeWord = 0wx36DB6D01 : W.t

      val bool   = 0wx4DB6DA41 : W.t
      val char   = 0wx5B6DB085 : W.t
      val int    = 0wx6DB6D405 : W.t
      val real   = 0wx8DB6D605 : W.t
      val string = 0wx9B6DB141 : W.t
      val word   = 0wxADB6D441 : W.t

      val word8  = 0wxB6DB6809 : W.t
      val word32 = 0wxCDB6D501 : W.t
(*
      val word64 = 0wxDB6DB101 : W.t
*)

      fun hole () = 0w0 : W.t

      open Arg TypeHashRep)
end
