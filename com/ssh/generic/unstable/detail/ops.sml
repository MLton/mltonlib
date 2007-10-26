(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ops = struct
   datatype 'a wops =
      W of {wordSize : Int.t,
            orb : 'a BinOp.t,
            << : 'a ShiftOp.t,
            ~>> : 'a ShiftOp.t,
            >> : 'a ShiftOp.t,
            isoWord8 : ('a, Word8.t) Iso.t,
            isoWord8X : ('a, Word8.t) Iso.t}

   datatype 'a iops =
      I of {precision : Int.t Option.t,
            maxInt : 'a Option.t,
            fromInt : Int.t -> 'a,
            *` : 'a BinOp.t,
            +` : 'a BinOp.t,
            div : 'a BinOp.t,
            mod : 'a BinOp.t}
end

functor MkWordOps (Arg : WORD) = struct
   local
      open Arg
   in
      val ops =
          Ops.W {wordSize = wordSize, orb = op orb,
                 << = op <<, ~>> = op ~>>, >> = op >>,
                 isoWord8 = isoWord8, isoWord8X = isoWord8X}
   end
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

functor MkIntOps (Arg : INTEGER) = struct
   local
      open Arg
   in
      val ops =
          Ops.I {precision = precision,
                 maxInt = maxInt,
                 fromInt = fromInt,
                 *` = op *, +` = op +,
                 div = op div, mod = op mod}
   end
end

structure FixedIntOps = MkIntOps (FixedInt)
structure IntOps = MkIntOps (Int)
structure LargeIntOps = MkIntOps (LargeInt)
