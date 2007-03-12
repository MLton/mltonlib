(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == C Stringable ==
 *
 * C stringables can be embedded into strings.  The string representation
 * is usually human readable and corresponds to C syntax and conventions.
 *
 * See also: {STRINGABLE}
 *)

signature CSTRINGABLE_CORE = sig
   type cstringable

   val embCString : (cstringable, String.t) Emb.t
   (**
    * An embedding of cstringables into strings.  It is always equivalent
    * to {(toCString, fromCString)}.
    *)
end

signature CSTRINGABLE_EX = sig
   type cstringable_ex

   val fromCString : String.t -> cstringable_ex Option.t
   (**
    * Returns {SOME v} if a cstringable {v} can be parsed from a prefix of
    * the given string, ignoring initial whitespace; {NONE} is returned
    * otherwise.  May raise an exception if a cstringable can be parsed
    * from the prefix, but the value is not representable as a cstringable
    * (e.g. causes {Overflow}).
    *)

   val toCString : cstringable_ex -> String.t
   (**
    * Returns a string containing a representation of the given
    * cstringable.
    *)
end

signature CSTRINGABLE = sig
   include CSTRINGABLE_CORE CSTRINGABLE_EX
   sharing type cstringable = cstringable_ex
end
