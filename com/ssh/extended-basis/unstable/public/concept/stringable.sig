(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Stringable ==
 *
 * Stringables can be embedded into strings.  The string representation is
 * usually human readable and corresponds to SML syntax and conventions.
 *
 * See also: {CSTRINGABLE}
 *)

signature STRINGABLE_CORE = sig
   type stringable

   val embString : (stringable, String.t) Emb.t
   (**
    * An embedding of stringables into strings.  It is always equivalent
    * to {(toString, fromString)}.
    *)
end

signature STRINGABLE_EX = sig
   type stringable_ex

   val fromString : String.t -> stringable_ex Option.t
   (**
    * Returns {SOME v} if a stringable {v} can be parsed from a prefix of
    * the given string, ignoring initial whitespace; {NONE} is returned
    * otherwise.  May raise an exception if a stringable can be parsed
    * from the prefix, but the value is not representable as a stringable
    * (e.g. causes {Overflow}).
    *)

   val toString : stringable_ex -> String.t
   (**
    * Returns a string containing a representation of the given
    * stringable.
    *)
end

signature STRINGABLE = sig
   include STRINGABLE_CORE STRINGABLE_EX
   sharing type stringable = stringable_ex
end
