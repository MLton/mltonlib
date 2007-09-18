(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for pretty-printing values of
 * arbitrary SML types.
 *
 * Features:
 * - Handles arbitrary cyclic data structures.
 * - Shows sharing.
 * - Output roughly as close to SML syntax as possible.
 *)
signature PRETTY = sig
   structure Pretty : OPEN_REP

   (** Substructure for specifying formatting options. *)
   structure Fmt : sig
      type t and 'a opt

      val default : t
      (** Default formatting options.  See the options for the defaults. *)

      (** == Updating Options ==
       *
       * Example:
       *
       *> let open Fmt in default & maxDepth := SOME 3
       *>                         & maxLength := SOME 10 end
       *)

      val & : t * ('a opt * 'a) -> t
      val := : ('a opt * 'a) UnOp.t

      (** == Querying Options ==
       *
       * Example:
       *
       *> let open Fmt in !maxDepth default end
       *)

      val ! : 'a opt -> t -> 'a

      (** == Options == *)

      val intRadix  : StringCvt.radix   opt (** default: {StringCvt.DEC} *)
      val wordRadix : StringCvt.radix   opt (** default: {StringCvt.HEX} *)
      val realFmt   : StringCvt.realfmt opt (** default: {StringCvt.GEN NONE} *)
      val maxDepth  : Int.t Option.t    opt (** default: {NONE} *)
      val maxLength : Int.t Option.t    opt (** default: {NONE} *)
      val maxString : Int.t Option.t    opt (** default: {NONE} *)
   end

   val fmt : ('a, 'x) Pretty.t -> Fmt.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val pretty : ('a, 'x) Pretty.t -> 'a -> Prettier.t
   (** {pretty t} is equivalent to {fmt t Fmt.default}. *)

   val show : ('a, 'x) Pretty.t -> 'a -> String.t
   (** {show t} is equivalent to {Prettier.render NONE o pretty t}. *)
end

signature PRETTY_CASES = sig
   include OPEN_CASES PRETTY
   sharing Rep = Pretty
end

signature WITH_PRETTY_DOM = HASH_CASES
