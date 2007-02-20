(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Stringables can be embedded into strings.  The string representation is
 * usually human readable.
 *)
signature STRINGABLE = sig
   type stringable

   val embString : (stringable, String.t) Emb.t
   (**
    * An embedding of stringables into strings.  It is always equivalent
    * to {(toString, fromString)}.
    *)

   val fromString : String.t -> stringable Option.t
   (**
    * Returns {SOME v} if a stringable {v} can be parsed from a prefix of
    * the given string, ignoring initial whitespace; {NONE} is returned
    * otherwise.  May raise an exception if a stringable can be parsed
    * from the prefix, but the value is not representable as a stringable
    * (e.g. causes {Overflow}).
    *)

   val toString : stringable -> String.t
   (**
    * Returns a string containing a representation of the given
    * stringable.
    *)
end
