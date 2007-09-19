(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for pretty-printing values of
 * arbitrary Standard ML types.
 *
 * Features:
 * - The result is a document that can be rendered to a desired width
 * (number of columns).
 * - The output is roughly as close to Standard ML syntax as possible.
 * - Eliminates unnecessary parentheses from the output.
 * - Can optionally pretty print only a part of the value (up to given
 * datatype depth and sequence or string length).  Partial data is
 * indicated as "\..." (an illegal escape sequence) in strings and as
 * "..." otherwise.
 * - The default formatting of integers, words, and reals can be
 * specified.
 * - The radix of integers and words is shown in the output with a "b"
 * (binary ; HaMLet-S), "o" (octal ; non-standard), or "x" prefix.
 * - Sharing of mutable objects is shown in the output.  Shared mutable
 * objects are assigned a sequence number, indicated by a "#n=" prefix at
 * the first occurrence.  Subsequent occurrences of the shared object are
 * indicated by a "#n".
 * - Handles arbitrary cyclic data structures.
 * - Supports pretty printing infix constructors in infix notation with a
 * given fixity.
 *)
signature PRETTY = sig
   structure PrettyRep : OPEN_REP

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
       *>                         & intRadix := StringCvt.HEX end
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

      (** == Options ==
       *
       * The defaults for scalar types have been chosen to match the
       * {X.toString} functions provided by the Basis library with the
       * exception.
       *)

      val intRadix  : StringCvt.radix   opt (** default: {StringCvt.DEC} *)
      val maxDepth  : Int.t Option.t    opt (** default: {NONE} *)
      val maxLength : Int.t Option.t    opt (** default: {NONE} *)
      val maxString : Int.t Option.t    opt (** default: {NONE} *)
      val realFmt   : StringCvt.realfmt opt (** default: {StringCvt.GEN NONE} *)
      val wordRadix : StringCvt.radix   opt (** default: {StringCvt.HEX} *)
   end

   (** Substructure for additional pretty printing combinators. *)
   structure Pretty : sig
      (** == Monadic Combinator Interface ==
       *
       * This interface allows the pretty printer stored in a type
       * representation to be extracted and replaced with a custom pretty
       * printer.
       *)

      include MONAD_CORE

      val getFmt : Fmt.t monad
      (** Returns the default formatting options. *)

      val setFmt : Fmt.t -> Unit.t monad
      (**
       * Functionally sets the default formatting options.  The new
       * default formatting options are only passed to the children of the
       * current monadic operation.  Note that changing the {maxDepth}
       * option has no effect on any default printers.
       *)

      val getRemDepth : Int.t Option.t monad
      (** Returns the remaining depth. *)

      val setRemDepth : Int.t Option.t -> Unit.t monad
      (**
       * Functionally sets the remaining depth.  The new depth only
       * affects the direct subactions of the current monadic action.
       *)

      structure Fixity : sig
         datatype t =
            ATOMIC
          | NONFIX
          | INFIXL of Int.t
          | INFIXR of Int.t
      end

      type 'a t = 'a -> (Fixity.t * Prettier.t) monad
      (** The type of pretty printing actions. *)

      val getPrinter : ('a, 'x) PrettyRep.t -> 'a t
      (** Returns the pretty printing action stored in a type representation. *)

      val setPrinter : 'a t -> ('a, 'x) PrettyRep.t UnOp.t
      (** Functionally updates the pretty printing action in a type rep. *)

      val mapPrinter : 'a t UnOp.t -> ('a, 'x) PrettyRep.t UnOp.t
      (** {mapPrinter f t} is equivalent to {setPrinter (f (getPrinter t)) t}. *)

      (** == Infix Constructors ==
       *
       * The {infixL} and {infixR} combinators update a given sum type
       * representation to print the value with an infix constructor.
       *
       * As an example, consider the following type representation
       * constructor definition:
       *
       *> local
       *>    val et = C "&"
       *> in
       *>    fun a &` b =
       *>        iso (data (Pretty.infixL 0 et (a, b)
       *>                    (C1 et (tuple2 (a, b)))))
       *>             (fn op & ? => ?, op &)
       *> end
       *
       * Now,
       *
       *> show (int &` int &` int) (1 & 2 & 3)
       *
       * would evaluate to
       *
       *> "1 & 2 & 3"
       *)

      val infixL : Int.t
                   -> Generics.Con.t
                   -> ('a, 'x) PrettyRep.t * ('b, 'y) PrettyRep.t
                   -> ('a * 'b, 'z) PrettyRep.s UnOp.t

      val infixR : Int.t
                   -> Generics.Con.t
                   -> ('a, 'x) PrettyRep.t * ('b, 'y) PrettyRep.t
                   -> ('a * 'b, 'z) PrettyRep.s UnOp.t
   end

   val fmt : ('a, 'x) PrettyRep.t -> Fmt.t -> 'a -> Prettier.t
   (** Extracts the prettifying function. *)

   val pretty : ('a, 'x) PrettyRep.t -> 'a -> Prettier.t
   (** {pretty t} is equivalent to {fmt t Fmt.default}. *)

   val show : ('a, 'x) PrettyRep.t -> 'a -> String.t
   (** {show t} is equivalent to {Prettier.render NONE o pretty t}. *)
end

signature PRETTY_CASES = sig
   include OPEN_CASES PRETTY
   sharing Rep = PrettyRep
end

signature WITH_PRETTY_DOM = HASH_CASES
