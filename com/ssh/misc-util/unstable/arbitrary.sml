(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A type-indexed function for generating random values of any type.  The
 * design is inspired by the QuickCheck library by Koen Claessen and John
 * Hughes:
 *
 *   http://www.cs.chalmers.se/~rjmh/QuickCheck/ .
 *)

signature ARBITRARY = sig
   type 'a arbitrary_t

   val arbitrary : 'a arbitrary_t -> 'a RanQD1Gen.gen
   (** Extracts the random value generator. *)

   val withGen : 'a RanQD1Gen.gen -> 'a arbitrary_t UnOp.t
   (** Functionally updates the random value generator. *)
end

functor LiftArbitrary
           (include ARBITRARY
            type 'a t
            val lift : ('a arbitrary_t, 'a t) Lift.t Thunk.t) : ARBITRARY =
struct
   type 'a arbitrary_t = 'a t
   val arbitrary = fn ? => Lift.get lift arbitrary ?
   val withGen = fn g => Lift.update lift (withGen g)
end

structure Arbitrary :> sig
   include STRUCTURAL_TYPE ARBITRARY
   sharing type arbitrary_t = t
end = struct
   structure G = RanQD1Gen and I = Int and R = Real and W = Word
         and Typ = TypeInfo

   datatype 'a t =
      IN of {gen : 'a G.gen,
             cog : int -> 'a -> G.t UnOp.t,
             typ : 'a Typ.t}
   type 'a arbitrary_t = 'a t

   val map = G.Monad.map
   val op >>= = G.>>=

   fun arbitrary (IN {gen, ...}) = gen
   fun withGen gen (IN {cog, typ, ...}) =
       IN {gen = gen, cog = cog, typ = typ}

   fun iso (IN {gen, cog, typ, ...}) (iso as (a2b, b2a)) =
       IN {gen = map b2a gen,
           cog = fn n => cog n o a2b,
           typ = Typ.iso typ iso}

   val unit = IN {gen = const (const ()),
                  cog = const (const (G.split 0w0)),
                  typ = Typ.unit}
   val bool = IN {gen = G.bool,
                  cog = const (G.split o (fn false => 0w1 | true => 0w2)),
                  typ = Typ.bool}
   val int  = IN {gen = map (fn w => W.toIntX (w - G.maxValue div 0w2))
                            (* XXX result may not fit an Int.int *)
                            (G.lift G.value),
                  cog = const (G.split o W.fromInt),
                  typ = Typ.int}
   val word = IN {gen = G.lift G.value,
                  cog = const G.split,
                  typ = Typ.word}
   val real = IN {gen = G.sized ((fn r => G.realInRange (~r, r)) o real),
                  cog = const (G.split o W.fromLarge o
                               PackWord32Little.subVec /> 0 o
                               PackReal32Little.toBytes o
                               Real32.fromLarge IEEEReal.TO_NEAREST o
                               R.toLarge),
                  typ = Typ.real}

   fun Y ? = let
      open Tie
      val genFn = pure (fn () => let
                              val r = ref (raising Fix.Fix)
                              fun f x = !r x
                           in
                              (G.resize (op div /> 2) f,
                               fn f' => (r := f' ; f'))
                           end)
   in
      iso (genFn *` function *` Typ.Y)
          (fn IN {gen = a, cog = b, typ = c} => a & b & c,
           fn a & b & c => IN {gen = a, cog = b, typ = c})
   end ?

   fun (IN {gen = aGen, cog = aCog, typ = aTyp, ...}) *`
       (IN {gen = bGen, cog = bCog, typ = bTyp, ...}) =
       IN {gen = G.Monad.>>& (aGen, bGen),
           cog = fn n => fn a & b => aCog n a o G.split 0w643 o bCog n b,
           typ = Typ.*` (aTyp, bTyp)}

   fun (IN {gen = aGen, cog = aCog, typ = aTyp, ...}) +`
       (IN {gen = bGen, cog = bCog, typ = bTyp, ...}) = let
      val aGen = map INL aGen
      val bGen = map INR bGen
      val gen = G.frequency [(Typ.numConsecutiveAlts aTyp, aGen),
                             (Typ.numConsecutiveAlts bTyp, bGen)]
      val gen0 = case Typ.hasBaseCase aTyp & Typ.hasBaseCase bTyp of
                    true & false => aGen
                  | false & true => bGen
                  | _            => gen
   in
      IN {gen = G.sized (fn 0 => gen0 | _ => gen),
          cog = fn n => fn INL a => G.split 0w423 o aCog n a
                         | INR b => G.split 0w324 o bCog n b,
          typ = Typ.+` (aTyp, bTyp)}
   end

   fun (IN {gen = aGen, cog = aCog, typ = aTyp, ...}) -->
       (IN {gen = bGen, cog = bCog, typ = bTyp, ...}) =
       IN {gen = G.promote (fn a => fn n => bGen n o aCog n a),
           cog = fn n => fn a2b => fn r =>
                    bCog n (a2b (aGen n (G.split 0w3 r))) (G.split 0w4 r),
           typ = Typ.--> (aTyp, bTyp)}

   val exn = let val e = Fail "Arbitrary.exn not supported yet"
             in IN {gen = raising e, cog = raising e, typ = Typ.exn}
             end
   fun regExn _ _ = ()

   fun list (IN {gen = xGen, cog = xCog, typ = xTyp, ...}) = let
      val xsGen = G.sized (0 <\ G.intInRange) >>= G.list xGen
      fun xsCog _ []      t = G.split 0w5 t
        | xsCog n (x::xs) t = xsCog n xs (xCog n x t)
   in
      IN {gen = xsGen, cog = xsCog, typ = Typ.list xTyp}
   end

   fun array a = iso (list a) Array.isoList (* XXX not quite right with Typ *)
   fun refc a = iso a (!, ref)              (* XXX not quite right with Typ *)

   fun vector a = iso (list a) Vector.isoList

   val char = IN {gen = map chr (G.intInRange (0, Char.maxOrd)),
                  cog = const (G.split o W.fromInt o ord),
                  typ = Typ.char}

   val string = iso (list char) String.isoList

   val largeInt  = iso int  (Iso.swap I.isoLarge)
   val largeWord = iso word (Iso.swap W.isoLarge)
   val largeReal = iso real (Iso.swap (R.isoLarge IEEEReal.TO_NEAREST))

   local
      fun mk large = iso word (Iso.<--> (Iso.swap W.isoLarge, large))
   in
      val word8  = mk Word8.isoLarge
      val word16 = mk Word16.isoLarge
      val word32 = mk Word32.isoLarge
      val word64 = mk Word64.isoLarge
   end
end
