(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Stream :> STREAM = struct
   datatype ('a, 's) step =
      DONE
    | GIVE of 'a * 's
    | SKIP of 's
   type ('a, 's) stream = 's * ('s -> ('a, 's) step)
   type 'a t = ('a, Univ.t) stream

   fun seal (s, s2xS) =
       case Univ.Iso.new ()
        of (to, from) =>
           (to s,
            (fn DONE        => DONE
              | SKIP s      => SKIP (to s)
              | GIVE (a, s) => GIVE (a, to s)) o s2xS o from)

   fun mapStep s2s (s, s2xS) = (s, s2s o s2xS)

   fun foldl xy2y y (u, u2s) = let
      fun lp (y, u) =
          case u2s u
           of DONE        => y
            | GIVE (x, u) => lp (xy2y (x, y), u)
            | SKIP u      => lp (y, u)
   in
      lp (y, u)
   end

   fun app ef = foldl (ef o #1) ()

   fun map x2y =
       mapStep (fn DONE        => DONE
                 | SKIP s      => SKIP s
                 | GIVE (x, s) => GIVE (x2y x, s))

   fun filter px =
       mapStep (fn GIVE (x, s) => if px x then GIVE (x, s) else SKIP s
                 | otherwise   => otherwise)

   fun tabulate (n, i2a) =
       if n < 0
       then raise Domain
       else seal (0,
                  fn i => if i < n
                          then GIVE (i2a i, i+1)
                          else DONE)

   fun unfoldr s2asO s =
       seal (s,
             fn s =>
                case s2asO s
                 of NONE        => DONE
                  | SOME (a, s) => GIVE (a, s))

   local
      fun mk length sub s = tabulate (length s, fn i => sub (s, i))
   in
      fun fromArray       ? = mk       Array.length       Array.sub ?
      fun fromVector      ? = mk      Vector.length      Vector.sub ?
      val fromString        = mk               size      String.sub
      fun fromArraySlice  ? = mk  ArraySlice.length  ArraySlice.sub ?
      fun fromVectorSlice ? = mk VectorSlice.length VectorSlice.sub ?
      val fromSubstring     = mk   Substring.length   Substring.sub
   end

   fun toBuffer s = case Buffer.new () of b => (app (Buffer.push b) s ; b)

   fun toArray  s = Buffer.toArray (toBuffer s)
   fun toVector s = Buffer.toVector (toBuffer s)
   fun toString s = Buffer.toString (toBuffer s)

   fun fromList xs = unfoldr List.getItem xs
   fun toList s = rev (foldl op :: [] s)
end
