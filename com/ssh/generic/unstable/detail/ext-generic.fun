(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor ExtGeneric (Arg : EXT_GENERIC_DOM) :>
   EXT_GENERIC_COD
      where type ('a, 'x) Ext.Index.t =
                 ('a, 'a Arg.New.Index.t * 'x) Arg.Ext.Index.t
      where type ('a, 'x) Ext.Index.s =
                 ('a, 'a Arg.New.Index.s * 'x) Arg.Ext.Index.s
      where type ('a, 'k, 'x) Ext.Index.p =
                 ('a, 'k, ('a, 'k) Arg.New.Index.p * 'x) Arg.Ext.Index.p =
struct
   (* <-- SML/NJ workaround *)
   open Fn
   (* SML/NJ workaround --> *)

   open Arg

   structure Ext : EXT_GENERIC = struct
      structure Index : EXT_GENERIC_INDEX = struct
         fun get get = Pair.snd o get
         fun map map f = map (Pair.map (id, f))

         type ('a, 'x) t = ('a, 'a New.Index.t * 'x) Ext.Index.t
         fun getT ? = get Ext.Index.getT ?
         fun mapT ? = map Ext.Index.mapT ?

         type ('a, 'x) s = ('a, 'a New.Index.s * 'x) Ext.Index.s
         fun getS ? = get Ext.Index.getS ?
         fun mapS ? = map Ext.Index.mapS ?

         type ('a, 'k, 'x) p = ('a, 'k, ('a, 'k) New.Index.p * 'x) Ext.Index.p
         fun getP ? = get Ext.Index.getP ?
         fun mapP ? = map Ext.Index.mapP ?
      end

      fun nullary ext new x = ext (new, x)
      fun unary ext new a x2y = ext a (Pair.map (new, x2y))
      fun binary ext new ab xy2z =
          ext ab (Pair.map (new, xy2z) o Pair.swizzle)
      fun morph ext new by aIb y2x =
          ext by aIb (Pair.map (flip new aIb, y2x))

      fun iso ? = morph Ext.iso New.iso ?
      fun isoProduct ? = morph Ext.isoProduct New.isoProduct ?
      fun isoSum ? = morph Ext.isoSum New.isoSum ?
      fun op *` ? = binary Ext.*` New.*` ?
      fun T ? = unary Ext.T New.T ?
      fun R l = unary (Ext.R l) (New.R l)
      fun tuple ? = unary Ext.tuple New.tuple ?
      fun record ? = unary Ext.record New.record ?
      fun op +` ? = binary Ext.+` New.+` ?
      fun C0 c = nullary (Ext.C0 c) (New.C0 c)
      fun C1 c = unary (Ext.C1 c) (New.C1 c)
      fun data ? = unary Ext.data New.data ?
      fun unit ? = nullary Ext.unit New.unit ?
      fun Y y = Ext.Y (Tie.tuple2 (New.Y, y))
      fun op --> ? = binary Ext.--> New.--> ?
      fun exn ? = nullary Ext.exn New.exn ?
      fun regExn a e x2ef =
          Ext.regExn a e (fn (a, x) => (New.regExn a e ; x2ef x))
      fun array ? = unary Ext.array New.array ?
      fun refc ? = unary Ext.refc New.refc ?
      fun vector ? = unary Ext.vector New.vector ?
      fun largeInt ? = nullary Ext.largeInt New.largeInt ?
      fun largeReal ? = nullary Ext.largeReal New.largeReal ?
      fun largeWord ? = nullary Ext.largeWord New.largeWord ?
      fun word8 ? = nullary Ext.word8 New.word8 ?
   (* fun word16 ? = nullary Ext.word16 New.word16 ?
      (* Word16 not provided by SML/NJ *) *)
      fun word32 ? = nullary Ext.word32 New.word32 ?
      fun word64 ? = nullary Ext.word64 New.word64 ?
      fun list ? = unary Ext.list New.list ?
      fun bool ? = nullary Ext.bool New.bool ?
      fun char ? = nullary Ext.char New.char ?
      fun int ? = nullary Ext.int New.int ?
      fun real ? = nullary Ext.real New.real ?
      fun string ? = nullary Ext.string New.string ?
      fun word ? = nullary Ext.word New.word ?
   end

   structure Gen : GENERIC = struct
      structure Index : GENERIC_INDEX = struct
         type 'a t = ('a, Unit.t) Ext.Index.t
         type 'a s = ('a, Unit.t) Ext.Index.s
         type ('a, 'k) p = ('a, 'k, Unit.t) Ext.Index.p
      end

      fun iso b aIb = Ext.iso b aIb ignore
      fun isoProduct b aIb = Ext.isoProduct b aIb ignore
      fun isoSum b aIb = Ext.isoSum b aIb ignore
      fun op *` ab = Ext.*` ab ignore
      fun T a = Ext.T a ignore
      fun R l a = Ext.R l a ignore
      fun tuple a = Ext.tuple a ignore
      fun record a = Ext.record a ignore
      fun op +` ab = Ext.+` ab ignore
      fun C0 c = Ext.C0 c ()
      fun C1 c a = Ext.C1 c a ignore
      fun data a = Ext.data a ignore
      val unit = Ext.unit ()
      fun Y ? = Ext.Y Tie.unit ?
      fun op --> ab = Ext.--> ab ignore
      val exn = Ext.exn ()
      fun regExn a e = Ext.regExn a e ignore
      fun array a = Ext.array a ignore
      fun refc a = Ext.refc a ignore
      fun vector a = Ext.vector a ignore
      val largeInt = Ext.largeInt ()
      val largeReal = Ext.largeReal ()
      val largeWord = Ext.largeWord ()
      val word8 = Ext.word8 ()
   (* val word16 = Ext.word16 () (* Word16 not provided by SML/NJ *) *)
      val word32 = Ext.word32 ()
      val word64 = Ext.word64 ()
      fun list a = Ext.list a ignore
      val bool = Ext.bool ()
      val char = Ext.char ()
      val int = Ext.int ()
      val real = Ext.real ()
      val string = Ext.string ()
      val word = Ext.word ()
   end
end
