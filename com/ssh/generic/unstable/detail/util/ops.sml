(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ops = struct
   datatype ('word, 'stream) w =
      W of {<< : 'word ShiftOp.t,
            >> : 'word ShiftOp.t,
            compare : 'word Cmp.t,
            isoLargeInt : ('word, LargeInt.t) Iso.t,
            scan : StringCvt.radix
                   -> (Char.t, 'stream) Reader.t
                   -> ('word, 'stream) Reader.t,
            isoWord : ('word, Word.t) Iso.t,
            isoWord8 : ('word, Word8.t) Iso.t,
            isoWord8X : ('word, Word8.t) Iso.t,
            mod : 'word BinOp.t,
            orb : 'word BinOp.t,
            wordSize : Int.t,
            ~>> : 'word ShiftOp.t}

   datatype ('int, 'stream) i =
      I of {*` : 'int BinOp.t,
            +` : 'int BinOp.t,
            compare : 'int Cmp.t,
            div : 'int BinOp.t,
            fmt : StringCvt.radix -> 'int -> String.t,
            isoInt : ('int, Int.t) Iso.t,
            isoLarge : ('int, LargeInt.t) Iso.t,
            maxInt : 'int Option.t,
            mod : 'int BinOp.t,
            precision : Int.t Option.t,
            scan : StringCvt.radix
                   -> (Char.t, 'stream) Reader.t
                   -> ('int, 'stream) Reader.t}

   datatype ('real, 'word, 'stream) r =
      R of {bitsOps : ('word, 'stream) w,
            bytesPerElem : Int.t,
            isoBits : ('real, 'word) Iso.t Option.t,
            scan : (Char.t, 'stream) Reader.t
                   -> ('real, 'stream) Reader.t,
            subArr : Word8Array.t * Int.t -> 'real,
            toBytes : 'real -> Word8Vector.t}

   datatype ('elem, 'list, 'result, 'seq, 'slice) s =
      S of {foldl : ('elem * 'result -> 'result) -> 'result -> 'seq -> 'result,
            fromList : 'list -> 'seq,
            getItem : 'slice -> ('elem * 'slice) Option.t,
            length : 'seq -> Int.t,
            sub : 'seq * Int.t -> 'elem,
            toSlice : 'seq -> 'slice}
end

functor MkWordOps (include WORD) = struct
   val ops = Ops.W {wordSize = wordSize, orb = op orb, << = op <<, ~>> = op ~>>,
                    >> = op >>, isoLargeInt = isoLargeInt, isoWord = isoWord,
                    isoWord8 = isoWord8, isoWord8X = isoWord8X, mod = op mod,
                    compare = compare, scan = scan}
end

structure LargeRealWordOps = MkWordOps (CastLargeReal.Bits)
structure LargeWordOps = MkWordOps (LargeWord)
structure RealWordOps = MkWordOps (CastReal.Bits)
structure WordOps = MkWordOps (Word)
structure Word32Ops = MkWordOps (Word32)
(*
structure Word64Ops = MkWordOps (Word64)
*)
structure Word8Ops = MkWordOps (Word8)

functor MkIntOps (include INTEGER) = struct
   val ops = Ops.I {precision = precision, maxInt = maxInt, isoInt = isoInt,
                    isoLarge = isoLarge, *` = op *, +` = op +, div = op div,
                    mod = op mod, scan = scan, fmt = fmt, compare = compare}
end

structure FixedIntOps = MkIntOps (FixedInt)
structure IntOps = MkIntOps (Int)
structure LargeIntOps = MkIntOps (LargeInt)

functor MkRealOps (structure Real : REAL
                   include CAST_REAL where type t = Real.t
                   include PACK_REAL where type real = Real.t
                   val ops : (Bits.t, 'stream) Ops.w) = struct
   val ops = Ops.R {bitsOps = ops, bytesPerElem = bytesPerElem,
                    isoBits = isoBits, scan = Real.scan, subArr = subArr,
                    toBytes = toBytes}
end

structure RealOps = MkRealOps (open CastReal PackRealLittle RealWordOps
                               structure Real = Real)
structure LargeRealOps =
   MkRealOps (open CastLargeReal PackLargeRealLittle LargeRealWordOps
              structure Real = LargeReal)

functor MkSeqOps (structure Seq : sig
                     type 'a t
                     val length : 'a t -> Int.t
                     val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
                     val fromList : 'a List.t -> 'a t
                     val sub : 'a t * Int.t -> 'a
                  end
                  structure Slice : sig
                     type 'a t
                     val full : 'a Seq.t -> 'a t
                     val getItem : 'a t -> ('a * 'a t) Option.t
                  end) = struct
   val ops = Ops.S {length = Seq.length, foldl = Seq.foldl,
                    toSlice = Slice.full, getItem = Slice.getItem,
                    fromList = Seq.fromList, sub = Seq.sub}
end

structure ArrayOps = MkSeqOps (structure Seq = Array and Slice = ArraySlice)
structure VectorOps = MkSeqOps (structure Seq = Vector and Slice = VectorSlice)
structure ListOps = MkSeqOps
  (structure Seq = struct
      open List
      val fromList = TopLevel.id
   end
   structure Slice = struct
      open List
      val full = TopLevel.id
   end)
structure StringOps = struct
   val ops = Ops.S {length = String.length, foldl = String.foldl,
                    toSlice = Substring.full, getItem = Substring.getc,
                    fromList = String.fromList, sub = String.sub}
end
