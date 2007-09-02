(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for pretty-printing values of
 * arbitrary SML datatypes.
 *
 * Features:
 * - Handles arbitrary cyclic data structures.
 * - Shows sharing.
 * - Output roughly as close to SML syntax as possible.
 *)
signature PRETTY = sig
   structure Pretty : OPEN_REP

   val pretty : ('a, 'x) Pretty.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val show : ('a, 'x) Pretty.t -> 'a -> String.t
   (** {show t} is equivalent to {Prettier.render NONE (pretty t)}. *)
end

signature PRETTY_CASES = sig
   include OPEN_CASES PRETTY
   sharing Rep = Pretty
end

signature WITH_PRETTY_DOM = HASH_CASES
