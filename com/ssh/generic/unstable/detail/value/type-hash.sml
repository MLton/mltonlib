(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTypeHash (Arg : OPEN_CASES) : TYPE_HASH_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   local
      open Word32
   in
      fun unary c h = h * 0w19 + c
      fun binary c (l, r) = l * 0w13 + r * 0w17 + c
      fun text toString =
          String.foldl (fn (c, h) => h * 0w33 + fromInt (ord c)) 0w5381 o
          toString
   end

   structure TypeHash = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = Word32.t))

   val typeHash = TypeHash.This.getT

   structure Layered = LayerCases
     (structure Outer = Arg and Result = TypeHash and Rep = TypeHash.Closed

      fun iso        ? _ = unary 0wxD00B6B6B ?
      fun isoProduct ? _ = unary 0wxC01B56DB ?
      fun isoSum     ? _ = unary 0wxB006B6DB ?

      fun op *`  ? = binary 0wx00ADB6DB ?
      fun T      ? = unary 0wx00B6DB6B ?
      fun R      l = unary (text Generics.Label.toString l)
      fun tuple  ? = unary 0wx00DB6DB5 ?
      fun record ? = unary 0wx01B6DB55 ?

      fun op +` ? = binary 0wx02DB6D4D ?
      val unit    = 0wx036DB6C5 : Word32.t
      fun C0    ? = text Generics.Con.toString ?
      fun C1    c = unary (text Generics.Con.toString c)
      fun data  ? = unary 0wx04DB6D63 ?

      fun Y ? = Tie.id (0wx05B6DB51 : Word32.t) ?

      fun op --> ? = binary 0wx06DB6D61 ?

      val exn = 0wx08DB6B69 : Word32.t
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list ? = unary 0wx09B6DB29 ?

      fun vector ? = unary 0wx0ADB6D29 ?

      fun array ? = unary 0wx0B6DB651 ?
      fun refc ? = unary 0wx0CDB6D51 ?

      val fixedInt = 0wx0DB6DAA1 : Word32.t
      val largeInt = 0wx1B6DB541 : Word32.t

      val largeReal = 0wx2DB6D851 : Word32.t
      val largeWord = 0wx36DB6D01 : Word32.t

      val bool   = 0wx4DB6DA41 : Word32.t
      val char   = 0wx5B6DB085 : Word32.t
      val int    = 0wx6DB6D405 : Word32.t
      val real   = 0wx8DB6D605 : Word32.t
      val string = 0wx9B6DB141 : Word32.t
      val word   = 0wxADB6D441 : Word32.t

      val word8  = 0wxB6DB6809 : Word32.t
      val word32 = 0wxCDB6D501 : Word32.t
      val word64 = 0wxDB6DB101 : Word32.t)

   open Layered
end
