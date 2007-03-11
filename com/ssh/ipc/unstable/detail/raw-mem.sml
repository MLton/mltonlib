(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RawMem :> sig
   structure Ptr : sig
      eqtype t
      val null : t
      val + : t * Word.t -> t
   end

   structure Type : sig
      type 'a t

      val size : 'a t -> Word.t
      val alignment : 'a t -> Word.t

      val iso : 'b t -> ('a, 'b) Iso.t -> 'a t

      type 'a p
      val tuple : 'a p -> 'a t
      val T : 'a t -> 'a p
      val *` : 'a p * 'b p -> ('a, 'b) Product.t p

      type 'a s
      val data : 'a s -> 'a t
      val C0 : Unit.t s
      val C1 : 'a t -> 'a s
      val +` : 'a s * 'b s -> ('a, 'b) Sum.t s

      val unit : Unit.t t

      val int8  : Int8.t  t
      val int16 : Int16.t t
      val int32 : Int32.t t
      val int64 : Int64.t t

      val word8  : Word8.t  t
      val word16 : Word16.t t
      val word32 : Word32.t t
      val word64 : Word64.t t

      val real32 : Real32.t t
      val real64 : Real64.t t
   end

   val get : 'a Type.t -> Ptr.t -> 'a
   val set : 'a Type.t -> Ptr.t -> 'a Effect.t
end = struct
   structure Word = struct
      open Word
      fun align (w, a) = (w + a - 0w1) andb ~a
   end

   structure Ptr = struct
      open MLton.Pointer
      val op + = MLton.Pointer.add
   end

   structure Type = struct
      datatype 'a t =
         I of {sz : Word.t, al : Word.t, rd : Ptr.t -> 'a,
               wr : Ptr.t -> 'a Effect.t}

      fun size (I {sz, ...}) = sz
      fun alignment (I {al, ...}) = al
      fun get (I {rd, ...}) = rd
      fun set (I {wr, ...}) = wr

      fun iso (I {sz, al, rd, wr}) (a2b, b2a) =
          I {sz = sz, al = al, rd = b2a o rd, wr = fn a => wr a o a2b}

      local
         open Ptr

         fun R get a = get (a, 0)
         fun W set a v = set (a, 0, v)
      in
         val unit = I {sz = 0w0, al = 0w1, rd = const (), wr = const ignore}

         val int8  = I {sz = 0w1, al = 0w1, rd = R getInt8,  wr = W setInt8}
         val int16 = I {sz = 0w2, al = 0w2, rd = R getInt16, wr = W setInt16}
         val int32 = I {sz = 0w4, al = 0w4, rd = R getInt32, wr = W setInt32}
         val int64 = I {sz = 0w8, al = 0w8, rd = R getInt64, wr = W setInt64}

         val word8  = I {sz = 0w1, al = 0w1, rd = R getWord8,  wr = W setWord8}
         val word16 = I {sz = 0w2, al = 0w2, rd = R getWord16, wr = W setWord16}
         val word32 = I {sz = 0w4, al = 0w4, rd = R getWord32, wr = W setWord32}
         val word64 = I {sz = 0w8, al = 0w8, rd = R getWord64, wr = W setWord64}

         val real32 = I {sz = 0w4, al = 0w4, rd = R getReal32, wr = W setReal32}
         val real64 = I {sz = 0w8, al = 0w8, rd = R getReal64, wr = W setReal64}
      end

      type 'a p = 'a t
      fun tuple (I {sz, al, rd, wr}) =
          I {sz = Word.align (sz, al), al = al, rd = rd, wr = wr}
      val T = id
      fun (I {sz=aS,al=aA,rd=aR,wr=aW}) *` (I {sz=bS,al=bA,rd=bR,wr=bW}) = let
         val d = Word.align (aS, bA)
      in
         I {sz = d+bS, al = Word.max (aA, bA),
            rd = fn p => aR p & bR (Ptr.+ (p, d)),
            wr = fn p => fn a & b => (aW p a ; bW (Ptr.+ (p, d)) b)}
      end

      datatype 'a s =
         S of {n : Int32.t, sz : Word.t, al : Word.t,
               rd : (Ptr.t -> 'a) Effect.t Effect.t,
               wr : Word.t * Int32.t -> 'a -> Ptr.t Effect.t}
      val tag = int32
      fun data (S {n, sz, al, rd, wr}) = let
         val d = Word.align (size tag, al)
         val al = Word.max (al, alignment tag)
         val rds = Array.array (n, undefined)
         val i = ref 0
      in
         rd (fn rd => (Array.update (rds, !i, rd) ; i := !i+1))
       ; I {sz = Word.align (sz + d, al), al = al, wr = flip (wr (d, 0)),
            rd = fn a => Array.sub (rds, get tag a) (Ptr.+ (a, d))}
      end
      val C0 = S {n = 1, sz = 0w0, al = 0w1, rd = pass (const ()),
                  wr = fn (_, i) => fn () => fn a => set tag a i}
      fun C1 (I {sz, al, rd, wr}) =
          S {n = 1, sz = sz, al = al, rd = pass rd,
             wr = fn (d, i) => fn v => fn a =>
                     (set tag a i ; wr (Ptr.+ (a, d)) v)}
      fun (S {n = aN, sz = aS, al = aA, rd = aR, wr = aW}) +`
          (S {n = bN, sz = bS, al = bA, rd = bR, wr = bW}) = let
         fun R r i s = r (fn r => s (i o r))
      in
         S {n = aN + bN, sz = Word.max (aS, bS), al = Word.max (aA, bA),
            rd = fn s => (R aR INL s ; R bR INR s),
            wr = fn (d, i) => Sum.sum (aW (d, i), bW (d, i + aN))}
      end
   end

   val get = Type.get
   val set = Type.set
end
