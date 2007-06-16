(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for pretty printing values of
 * arbitrary SML datatypes.  See [http://mlton.org/TypeRepedValues]
 * for further discussion.
 *)
signature SHOW = sig
   structure Show : OPEN_GENERIC_REP

   val layout : ('a, 'x) Show.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val show : Int.t Option.t -> ('a, 'x) Show.t -> 'a -> String.t
   (** {show m t = Prettier.pretty m o layout t} *)
end

signature SHOW_GENERIC = sig
   include OPEN_GENERIC SHOW
   sharing Rep = Show
end
