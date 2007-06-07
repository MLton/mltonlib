(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a type-indexed function for pretty printing values of
 * arbitrary SML datatypes.  See [http://mlton.org/TypeIndexedValues]
 * for further discussion.
 *)
signature SHOW = sig
   structure Show : EXT_GENERIC_INDEX

   val layout : ('a, 'x) Show.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val show : Int.t Option.t -> ('a, 'x) Show.t -> 'a -> String.t
   (** {show m t = Prettier.pretty m o layout t} *)
end

signature SHOW_GENERIC = sig
   include SHOW EXT_GENERIC
   sharing Show = Index
end
