(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic read function.
 *
 * Spaces and SML-style comments are skipped implicitly.
 *
 * Functions cannot be read.
 *)
signature READ = sig
   structure ReadRep : OPEN_REP

   val reader :
       ('a, 'x) ReadRep.t -> (Char.t, 'b) Reader.t -> 'b -> ('b, 'a * 'b) Sum.t
   (**
    * Parses a value of type {'a} from the given stream of type {'b}.
    * Returns either the stream at a position where a parse error was
    * detected or the parsed value and the stream at a position
    * immediately after the parsed value.  Other errors (e.g. {Overflow})
    * cause exceptions being raised.
    *
    * Note that parsing stops immediately after a valid value has been
    * parsed.  Any characters, spaces or otherwise, following a valid
    * value are ignored.
    *)

   val read : ('a, 'x) ReadRep.t -> String.t -> 'a
   (**
    * Parses a value of type {'a} from the given string.  Parse and other
    * errors (e.g. {Overflow}) cause exceptions being raised.  Parsing is
    * considered to fail unless the whole string is consumed.  Spaces and
    * SML-style comments are consumed implicitly.
    *)
end

signature READ_CASES = sig
   include CASES READ
   sharing Open.Rep = ReadRep
end

signature WITH_READ_DOM = TYPE_INFO_CASES
