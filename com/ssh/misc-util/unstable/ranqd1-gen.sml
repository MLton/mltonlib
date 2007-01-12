(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A quick-and-dirty random generator.
 *)

structure RanQD1Gen :> sig
   include RANDOM_GEN
   val make : Word32.t -> t
end = struct
   structure G =
      MkRandomGen
         (type t = Word32.t
          val (value, seed) = Iso.<--> (Iso.swap Word.isoLarge, Word32.isoLarge)
          val next = Misc.ranqd1
          fun split w = #2 o Misc.psdes /> seed w
          val maxValue = value Word32.maxWord)
   open G
   val make = id
end
