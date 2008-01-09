(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic read function.
 *)
signature READ = sig
   structure ReadRep : OPEN_REP

   val read : ('a, 'x) ReadRep.t -> String.t -> 'a
end

signature READ_CASES = sig
   include CASES READ
   sharing Open.Rep = ReadRep
end

signature WITH_READ_DOM = CASES
