(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ops = struct
   datatype 'a wops =
      W of {<< : 'a ShiftOp.t,
            >> : 'a ShiftOp.t,
            isoWord8 : ('a, Word8.t) Iso.t,
            isoWord8X : ('a, Word8.t) Iso.t,
            orb : 'a BinOp.t,
            wordSize : Int.t,
            ~>> : 'a ShiftOp.t}

   datatype 'a iops =
      I of {*` : 'a BinOp.t,
            +` : 'a BinOp.t,
            div : 'a BinOp.t,
            fromInt : Int.t -> 'a,
            maxInt : 'a Option.t,
            mod : 'a BinOp.t,
            precision : Int.t Option.t}

   datatype ('elem, 'list, 'result, 'seq, 'slice) sops =
      S of {foldl : ('elem * 'result -> 'result) -> 'result -> 'seq -> 'result,
            fromList : 'list -> 'seq,
            getItem : 'slice -> ('elem * 'slice) Option.t,
            length : 'seq -> Int.t,
            sub : 'seq * Int.t -> 'elem,
            toSlice : 'seq -> 'slice}
end

functor MkWordOps (include WORD) = struct
   val ops = Ops.W {wordSize = wordSize, orb = op orb, << = op <<, ~>> = op ~>>,
                    >> = op >>, isoWord8 = isoWord8, isoWord8X = isoWord8X}
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
   val ops = Ops.I {precision = precision, maxInt = maxInt, fromInt = fromInt,
                    *` = op *, +` = op +, div = op div, mod = op mod}
end

structure FixedIntOps = MkIntOps (FixedInt)
structure IntOps = MkIntOps (Int)
structure LargeIntOps = MkIntOps (LargeInt)

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
