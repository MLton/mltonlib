(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature FMAP = sig
   structure FmapRep : OPEN_REP

   structure Fmap : sig
      type 'a i and 'a t
      val get : (('a, 'x) FmapRep.t -> 'a i) t
      val map : ('a i UnOp.t -> ('a, 'x) FmapRep.t UnOp.t) t
   end
end

signature FMAP_CASES = sig
   include CASES FMAP
   sharing Open.Rep = FmapRep
end

signature WITH_FMAP_DOM = CASES

signature MK_FMAP_DOM = sig
   include FMAP_CASES
   type 'a t
   val t : ('a, Unit.t) Open.Rep.t -> ('a t, Unit.t) Open.Rep.t
end
