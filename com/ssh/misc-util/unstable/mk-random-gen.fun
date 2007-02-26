(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A functor for making random value generator combinators from a module
 * providing a random number generator.
 *)

functor MkRandomGen (RNG : RNG) :>
   RANDOM_GEN
      where type t = RNG.t = struct
   structure D = MkDbg (open DbgDefs val name = "MkRandomGen")
         and A = Array and R = Real and V = Vector and W = Word

   open RNG
   type 'a gen = Int.t -> t -> 'a

   val lift = const

 (*fun prj gb b2a n = b2a o gb n*)

   structure Monad =
      MkMonad (type 'a monad = 'a gen
               fun return a _ _ = a
               fun (m >>= k) n r = k (m n (split 0w314 r)) n (split 0w159 r))

   open Monad

   fun promote a2b n r a = a2b a n r
   fun sized i2g n r = i2g n n r
   fun resize f g = g o f
   fun bool _ r = maxValue div 0w2 < value r

   fun inRange bInRange (a2b, b2a) =
       map b2a o bInRange o Pair.map (Sq.mk a2b)

   fun wordInRange (l, h) =
       (D.assert 0 (fn () => l <= h)
      ; let val n = h - l + 0w1     (* XXX may overflow *)
            val d = maxValue div n  (* XXX may result in zero *)
            val m = n * d
        in lift (fn r => value r mod m div d + l)
        end)

   fun intInRange (l, h) =
       (D.assert 0 (fn () => l <= h)
      ; map (op + /> l)
            (inRange wordInRange (Iso.swap W.isoInt) (0, h - l)))

   local
      val w2r = R.fromLargeInt o W.toLargeInt
   in
      fun realInRange (l, h) =
          (D.assert 0 (fn () => l <= h)
         ; let val m = (h - l) / w2r maxValue
           in const (fn r => w2r (value r) * m + l)
           end)
   end

   fun elements xs =
       let val xs = V.fromList xs
       in map (xs <\ V.sub) (intInRange (0, V.length xs))
       end

   fun oneOf gs = elements gs >>= id

   fun frequency xs = let
      val xs = A.fromList xs
      val tot = A.foldli (fn (i, (n, g), tot) =>
                             (A.update (xs, i, (n+tot, g)) ; n+tot))
                         0 xs
      fun pick i n = let
         val (k, x) = A.sub (xs, i)
      in
         if n <= k then x else pick (i+1) n
      end
   in
      intInRange (1, tot) >>= pick 0
   end

   local
      fun unfold px sx x2y = let
         fun lp ys x =
             if px x then
                rev ys
             else
                lp (x2y x::ys) (sx x)
      in
         lp []
      end
   in
      fun list ga m n r =
          unfold (op = /> 0w0)
                 (op - /> 0w1)
                 (ga n o flip split r)
                 (W.fromInt m)
   end
end
