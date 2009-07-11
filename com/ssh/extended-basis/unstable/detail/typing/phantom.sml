(* Copyright (C) 2007-2009 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Phantom :> PHANTOM = struct
   type yes = unit
   type no  = unit

   structure Bool = struct
      type ('f, 't, 'r) t = unit
      type ('f, 't) T = unit
      type ('f, 't) F = unit
      val t = ()
      val f = ()
      fun split () = ((), ())
      val generalize = ignore
      fun iff _ = raise Fail "Phantom.Bool.iff"
      val notb = ignore
      val op andb = ignore
      val op orb = ignore
   end
end
