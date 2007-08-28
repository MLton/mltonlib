(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for pretty printing values of
 * arbitrary SML datatypes.
 *)
signature PRETTY = sig
   structure Pretty : OPEN_REP

   val layout : ('a, 'x) Pretty.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val pretty : Int.t Option.t -> ('a, 'x) Pretty.t -> 'a -> String.t
   (** {pretty m t} is equivalent to {Prettier.pretty m o layout t}. *)

   val show : ('a, 'x) Pretty.t -> 'a -> String.t
   (** {show t} is equivalent to {pretty NONE t}. *)
end

signature PRETTY_CASES = sig
   include OPEN_CASES PRETTY
   sharing Rep = Pretty
end
