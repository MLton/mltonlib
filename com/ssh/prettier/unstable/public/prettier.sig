(* Copyright (C) 2008 Vesa Karvonen
 * Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a pretty-printing library.  The design is primarily based
 * on Philip Wadler's article ``A prettier printer''
 *
 *   [http://homepages.inf.ed.ac.uk/wadler/topics/language-design.html]
 *
 * which is a redesign of John Hughes's pretty-printing library described
 * in ``The Design of a Pretty-Printing Library''
 *
 *   [http://www.cs.chalmers.se/~rjmh/Papers/pretty.html]
 *
 * Some of Daan Leijen's PPrint library
 *
 *   [http://www.cs.uu.nl/~daan/pprint.html]
 *
 * has also been implemented.
 *)
signature PRETTIER = sig
   type t
   (** The abstract type of documents. *)

   val renderer : Int.t Option.t -> (String.t -> (Unit.t, 's) IOSMonad.t)
                                 -> (t        -> (Unit.t, 's) IOSMonad.t)
   (** Function for linearizing a document directly to a given stream. *)

   val render : Int.t Option.t -> t -> String.t
   (** Renders the document as a string. *)

   val output : TextIO.outstream -> Int.t Option.t -> t Effect.t
   (** Writes the document to the output stream. *)

   val println : Int.t Option.t -> t Effect.t
   (** Writes the document to stdOut with a newline and flushes stdOut. *)

   (** == Basic Combinators == *)

   val empty : t
   (** The empty document is semantically equivalent to {txt ""}. *)

   val chr : Char.t -> t
   (**
    * {chr c} contains the character {c}.  The character shouldn't be a
    * newline.
    *)

   val txt : String.t -> t
   (**
    * {txt s} contains the string {s}.  The string shouldn't contain any
    * newline characters.
    *)

   val str : String.t -> t
   (**
    * Converts a simple preformatted string into a document.  The idea is
    * that newlines separate paragraphs and spaces after a newline specify
    * indentation.  Spaces inside paragraph are replaced by softlines,
    * paragraphs are prefixed with a line, nested by the specified
    * indentation level, and grouped.  Everything is then concatenated
    * together.
    *)

   val <^> : t BinOp.t
   (**
    * {l <^> r} is the concatenation of the documents {l} and {r}.
    *
    * Note: This is the same as the operator <> used in the original
    * Haskell libraries.  In SML, <> is already used.
    *)

   val nest : Int.t -> t UnOp.t
   (**
    * {nest n d} renders document {d} indented by {n} more columns.
    *
    * Note that in order for {nest} to have any effect, you must have line
    * breaks in {group}s in {d}.
    *)

   val line : t
   (**
    * Advances to the next line and indents, unless undone by {group} in
    * which case {line} behaves like {txt " "}.
    *)

   val linebreak : t
   (**
    * Advances to the next line and indents, unless undone by {group} in
    * which case {linebreak} behaves like {empty}.
    *)

   val group : t UnOp.t
   (**
    * Used to specify alternative layouts.  {group d} undoes all line
    * breaks in document {d}.  The resulting line of text is added to the
    * current output line if it fits.  Otherwise, the document is rendered
    * without changes (with line breaks).
    *)

   val choice : {wide : t, narrow : t} -> t
   (**
    * Used to specify alternative documents.  The wider document is added
    * to the current output line if it fits.  Otherwise, the narrow
    * document is rendered.
    *
    * Warning: This operation allows one to create documents whose
    * rendering may not produce optimal or easily predictable results.
    *)

   val lazy : t Thunk.t -> t
   (**
    * Creates a lazily computed document.  {lazy (fn () => doc)} is
    * equivalent to {doc} except that the expression {doc} may not be
    * evaluated at all.
    *
    * Note: This is primarily useful for specifying the narrow alternative
    * to {choice} - unless, of course, there is a chance that the whole
    * document will not be rendered at all.
    *)

   val softline : t
   (**
    * Behaves like a space if the resulting output fits, otherwise behaves
    * like {line}.
    *)

   val softbreak : t
   (**
    * Behaves like {empty} if the resulting output fits, otherwise behaves
    * like {line}.
    *)

   (** == Alignment Combinators == *)

   val column : (Int.t -> t) -> t
   val nesting : (Int.t -> t) -> t

   val indent : Int.t -> t UnOp.t
   val hang : Int.t -> t UnOp.t
   val align : t UnOp.t

   val width : (Int.t -> t) -> t UnOp.t

   val fillBreak : Int.t -> t UnOp.t
   val fill : Int.t -> t UnOp.t

   (** == Operators == *)

   val <+>  : t BinOp.t  (** Concatenates with a {space}. *)
   val <$>  : t BinOp.t  (** Concatenates with a {line}. *)
   val </>  : t BinOp.t  (** Concatenates with a {softline}. *)
   val <$$> : t BinOp.t  (** Concatenates with a {linebreak}. *)
   val <//> : t BinOp.t  (** Concatenates with a {softbreak}. *)

   (** == List Combinators == *)

   val sep : t List.t -> t  (** {sep = group o vsep} *)
   val cat : t List.t -> t  (** {cat = group o vcat} *)

   val punctuate : t -> t List.t UnOp.t
   (**
    * {punctuate sep docs} concatenates {sep} to the right of each
    * document in {docs} except the last one.
    *)

   val hsep    : t List.t -> t  (** Concatenates with {<+>}. *)
   val vsep    : t List.t -> t  (** Concatenates with {<$>}. *)
   val fillSep : t List.t -> t  (** Concatenates with {</>}. *)
   val hcat    : t List.t -> t  (** Concatenates with {<^>}. *)
   val vcat    : t List.t -> t  (** Concatenates with {<$$>}. *)
   val fillCat : t List.t -> t  (** Concatenates with {<//>}. *)

   (** == Bracketing Combinators == *)

   val enclose : t Sq.t -> t UnOp.t
   (** {enclose (l, r) d = l <^> d <^> r} *)

   val squotes  : t UnOp.t  (** {squotes  = enclose (squote, squote)} *)
   val dquotes  : t UnOp.t  (** {dquotes  = enclose (dquote, dquote)} *)
   val parens   : t UnOp.t  (** {parens   = enclose (lparen, rparen)} *)
   val angles   : t UnOp.t  (** {angles   = enclose (langle, rangle)} *)
   val braces   : t UnOp.t  (** {braces   = enclose (lbrace, rbrace)} *)
   val brackets : t UnOp.t  (** {brackets = enclose (lbracket, rbracket)} *)

   (** == Character Documents == *)

   val lparen    : t  (** {txt "("} *)
   val rparen    : t  (** {txt ")"} *)
   val langle    : t  (** {txt "<"} *)
   val rangle    : t  (** {txt ">"} *)
   val lbrace    : t  (** {txt "{"} *)
   val rbrace    : t  (** {txt "}"} *)
   val lbracket  : t  (** {txt "["} *)
   val rbracket  : t  (** {txt "]"} *)
   val squote    : t  (** {txt "'"} *)
   val dquote    : t  (** {txt "\""} *)
   val semi      : t  (** {txt ";"} *)
   val colon     : t  (** {txt ":"} *)
   val comma     : t  (** {txt ","} *)
   val space     : t  (** {txt " "} *)
   val dot       : t  (** {txt "."} *)
   val backslash : t  (** {txt "\\"} *)
   val equals    : t  (** {txt "="} *)
end
