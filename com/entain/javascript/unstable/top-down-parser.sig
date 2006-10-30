(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature TOP_DOWN_PARSER_STRUCTS = 
   sig
   end

signature TOP_DOWN_PARSER = 
   sig
      include TOP_DOWN_PARSER_STRUCTS

      structure Terminal:
         sig
            type t

            val equals: t * t -> bool
            val layout: t -> Layout.t
            val new: string -> t
         end

      type ('a , 'b) t

      val delayDef: {name: string} -> ('a, 'b) t * {define: ('a, 'b) t -> unit}
      val empty: unit -> ('a, unit) t
      val oneOrMore: ('a, 'b) t -> ('a, 'b list) t
      val or: ('a, 'b) t list -> ('a, 'b) t
      val orB: ('a, 'b) t list -> ('a, 'b) t (* Allow backtracking. *)
      val opt: ('a, 'b) t -> ('a, 'b option) t
      val parse:
         ('a, 'b) t
         -> {insert: (Terminal.t * 'a) option -> (Terminal.t * 'a) option,
             stream: (Terminal.t * 'a) Stream.t}
         -> 'b option
      val recur: (('a, 'b) t -> ('a, 'b) t) -> ('a, 'b) t
      val showConsider: bool ref
      val seq: ('a, 'b) t list -> ('a, 'b list) t
      val seq1: ('a, 'b1) t * ('b1 -> 'c) -> ('a, 'c) t
      val seq2:
         ('a, 'b1) t * ('a, 'b2) t * ('b1 * 'b2 -> 'c) -> ('a, 'c) t
      val seq3:
         ('a, 'b1) t * ('a, 'b2) t * ('a, 'b3) t
         * ('b1 * 'b2 * 'b3 -> 'c)
         -> ('a, 'c) t
      val seq4:
         ('a, 'b1) t * ('a, 'b2) t * ('a, 'b3) t * ('a, 'b4) t
         * ('b1 * 'b2 * 'b3 * 'b4 -> 'c)
         -> ('a, 'c) t
      val seq5:
         ('a, 'b1) t * ('a, 'b2) t * ('a, 'b3) t * ('a, 'b4) t
         * ('a, 'b5) t
         * ('b1 * 'b2 * 'b3 * 'b4 * 'b5 -> 'c)
         -> ('a, 'c) t
      val seq6:
         ('a, 'b1) t * ('a, 'b2) t * ('a, 'b3) t * ('a, 'b4) t
         * ('a, 'b5) t * ('a, 'b6) t
         * ('b1 * 'b2 * 'b3 * 'b4 * 'b5 * 'b6 -> 'c)
         -> ('a, 'c) t
      val seq7:
         ('a, 'b1) t * ('a, 'b2) t * ('a, 'b3) t * ('a, 'b4) t
         * ('a, 'b5) t * ('a, 'b6) t * ('a, 'b7) t
         * ('b1 * 'b2 * 'b3 * 'b4 * 'b5 * 'b6 * 'b7 -> 'c)
         -> ('a, 'c) t
      val terminal: Terminal.t -> ('a, 'a) t
      val zeroOrMore: ('a, 'b) t -> ('a, 'b list) t
   end
