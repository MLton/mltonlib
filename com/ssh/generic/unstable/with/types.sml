(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic
in
   structure Rep = Rep
   structure TopLevel = struct
      open TopLevel
      val C = C
      val C0 = C0
      val C0' = C0'
      val C1 = C1
      val C1' = C1'
      val L = L
      val R = R
      val R' = R'
      val T = T
      val Y = Y
      val data = data
      val data' = data'
      val iso = iso
      val op *` = op *`
      val op +` = op +`
      val op --> = op -->
      val record = record
      val record' = record'
      val tuple = tuple
      val tuple' = tuple'
   end
   open TopLevel
   structure Array = struct open Array val t = Generic.array end
   structure Sq = struct open Sq val t = Generic.sq end
   structure BinFn = struct
      open BinFn
      fun t (a, b) : ('a, 'b) t Rep.t = Sq.t a --> b
   end
   structure BinOp = struct open BinOp val t = Generic.binOp end
   structure Bool = struct open Bool val t = Generic.bool end
   structure CPS = struct
      open CPS
      fun t (a, c) : ('a, 'c) t Rep.t = (a --> c) --> c
   end
   structure Char = struct open Char val t = Generic.char end
   structure Order = struct open Order val t = Generic.order end
   structure Cmp = struct open Cmp fun t a : 'a t Rep.t = Sq.t a --> Order.t end
   structure Unit = struct open Unit val t = Generic.unit end
   structure Effect = struct open Effect fun t a : 'a t Rep.t = a --> Unit.t end
   structure Exn = struct open Exn val t = Generic.exn end
   structure FixedInt = struct open FixedInt val t = Generic.fixedInt end
   structure Fn = struct open Fn val t = Generic.--> end
   structure Int = struct open Int val t = Generic.int end
   structure Int32 = struct open Int32 val t = Generic.int32 end
   structure LargeInt = struct open LargeInt val t = Generic.largeInt end
   structure LargeReal = struct open LargeReal val t = Generic.largeReal end
   structure LargeWord = struct open LargeWord val t = Generic.largeWord end
   structure List = struct open List val t = Generic.list end
   structure Option = struct open Option val t = Generic.option end
   structure Pair = struct open Pair val t = Generic.tuple2 end
   structure Position = struct open Position val t = Generic.position end
   structure Product = struct open Product val t = Generic.&` end
   structure Reader = struct
      open Reader
      fun t (a, s) : ('a, 's) t Rep.t = s --> Option.t (Pair.t (a, s))
   end
   structure Real = struct open Real val t = Generic.real end
   structure Ref = struct open Ref val t = Generic.refc end
   structure Word = struct open Word val t = Generic.word end
   structure ShiftOp = struct
      open ShiftOp
      fun t a : 'a t Rep.t = Pair.t (a, Word.t) --> a
   end
   structure String = struct open String val t = Generic.string end
   structure Sum = struct open Sum val t = Generic.|` end
   structure Thunk = struct open Thunk fun t a : 'a t Rep.t = Unit.t --> a end
   structure UnOp = struct open UnOp val t = Generic.unOp end
   structure UnPr = struct open UnPr fun t a : 'a t Rep.t = a --> Bool.t end
   structure Vector = struct open Vector val t = Generic.vector end
   structure Word32 = struct open Word32 val t = Generic.word32 end
   structure Word8 = struct open Word8 val t = Generic.word8 end
   structure Word8Vector = struct
      open Word8Vector
      val t = iso (Vector.t Word8.t) isoPoly
   end
end
