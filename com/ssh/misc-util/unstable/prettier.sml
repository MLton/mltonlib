(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A pretty-printing library.  The design is primarily based on Philip
 * Wadler's article ``A prettier printer''
 *
 *   http://homepages.inf.ed.ac.uk/wadler/topics/language-design.html
 *
 * which is a redesign of John Hughes's pretty-printing library described
 * in ``The Design of a Pretty-Printing Library''
 *
 *   http://www.cs.chalmers.se/~rjmh/Papers/pretty.html
 *
 * Some of Daan Leijen's PPrint library
 *
 *   http://www.cs.uu.nl/~daan/pprint.html
 *
 * has also been implemented.
 *)

structure Prettier :> sig
   type t
   (** The abstract type of documents. *)

   datatype elem =
      STRING of String.t
    | NEWLINE of Int.t

   val fold : (elem * 'a -> 'a) -> 'a -> Int.t Option.t -> t -> 'a
   (**
    * Linearizes the given document and folds the linearized document with
    * the given function.
    *)

   val app : elem Effect.t -> Int.t Option.t -> t Effect.t
   (** {app e = fold (e o #1) ()} *)

   val pretty : Int.t Option.t -> t -> String.t
   (** {pretty n d = concat (rev (fold op:: [] n d))} *)

   val println : TextIO.outstream -> Int.t Option.t -> t Effect.t
   (**
    * Writes the document to the specified stream with a newline and
    * flushes the stream.
    *)

   (** == BASIC COMBINATORS == *)

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

   (** == ALIGNMENT COMBINATORS == *)

   val column : (Int.t -> t) -> t
   val nesting : (Int.t -> t) -> t

   val indent : Int.t -> t UnOp.t
   val hang : Int.t -> t UnOp.t
   val align : t UnOp.t

   val width : (Int.t -> t) -> t UnOp.t

   val fillBreak : Int.t -> t UnOp.t
   val fill : Int.t -> t UnOp.t

   (** == OPERATORS == *)

   val <+>  : t BinOp.t  (** Concatenates with a {space}. *)
   val <$>  : t BinOp.t  (** Concatenates with a {line}. *)
   val </>  : t BinOp.t  (** Concatenates with a {softline}. *)
   val <$$> : t BinOp.t  (** Concatenates with a {linebreak}. *)
   val <//> : t BinOp.t  (** Concatenates with a {softbreak}. *)

   (** == LIST COMBINATORS == *)

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

   (** == BRACKETING COMBINATORS == *)

   val enclose : t Sq.t -> t UnOp.t
   (** {enclose (l, r) d = l <^> d <^> r} *)

   val squotes  : t UnOp.t  (** {squotes  = enclose (squote, squote)} *)
   val dquotes  : t UnOp.t  (** {dquotes  = enclose (dquote, dquote)} *)
   val parens   : t UnOp.t  (** {parens   = enclose (lparen, rparen)} *)
   val angles   : t UnOp.t  (** {angles   = enclose (langle, rangle)} *)
   val braces   : t UnOp.t  (** {braces   = enclose (lbrace, rbrace)} *)
   val brackets : t UnOp.t  (** {brackets = enclose (lbracket, rbracket)} *)

   (** == CHARACTER DOCUMENTS == *)

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
end = struct
   structure Dbg = MkDbg (open DbgDefs val name = "Prettier")
         and C = Char and S = String and SS = Substring

   val E = eager
   val F = force
   val L = lazy

   datatype t' =
      EMPTY
    | LINE of bool
    | JOIN of t Sq.t
    | NEST of Int.t * t
    | TEXT of String.t
    | CHOICE of {wide : t, narrow : t}
    | COLUMN of Int.t -> t
    | NESTING of Int.t -> t
   withtype t = t' Lazy.t

   datatype elem =
      STRING of String.t
    | NEWLINE of Int.t

   val lazy = L

   val empty = E EMPTY
   val line = E (LINE false)
   val linebreak = E (LINE true)
   val column = E o COLUMN
   val nesting = E o NESTING

   local
      fun assertAllPrint str =
          Dbg.assert 0 (fn () => S.all C.isPrint str)
   in
      val txt' = E o TEXT
      val txt = txt' o Effect.obs assertAllPrint
      val chr = txt o str
   end

   val parens   as (lparen,   rparen)   = (txt' "(", txt' ")")
   val angles   as (langle,   rangle)   = (txt' "<", txt' ">")
   val braces   as (lbrace,   rbrace)   = (txt' "{", txt' "}")
   val brackets as (lbracket, rbracket) = (txt' "[", txt' "]")
   val squote    = txt' "'"
   val dquote    = txt' "\""
   val semi      = txt' ";"
   val colon     = txt' ":"
   val comma     = txt' ","
   val space     = txt' " "
   val dot       = txt' "."
   val backslash = txt' "\\"
   val equals    = txt' "="

   val op <^> = E o JOIN

   fun punctuate sep =
       fn [] => []
        | d::ds => let
          fun lp rs d1 =
              fn [] => List.revAppend (rs, [d1])
               | d2::ds => lp (d1 <^> sep::rs) d2 ds
       in
          lp [] d ds
       end

   fun nest n = E o n <\ NEST

   fun spaces n = S.tabulate (n, const #" ")

   fun align d = column (fn k => nesting (fn i => nest (k-i) d))
   fun hang i d = align (nest i d)
   fun indent i d = hang i (txt (spaces i) <^> d)

   fun width f d = column (fn l => d <^> column (fn r => f (r-l)))

   local
      fun mk p t f =
          width (fn w => if p (f, w) then t f else txt (spaces (f-w)))
   in
      val fillBreak = mk op <  (flip nest linebreak)
      val fill      = mk op <= (const empty)
   end

   local
      fun flatten doc =
          L (fn () =>
                case F doc of
                   EMPTY =>
                   doc
                 | JOIN (lhs, rhs) =>
                   E (JOIN (flatten lhs, flatten rhs))
                 | NEST (cols, doc) =>
                   E (NEST (cols, flatten doc))
                 | TEXT _ =>
                   doc
                 | LINE b =>
                   if b then empty else space
                 | CHOICE {wide, ...} =>
                   wide
                 | COLUMN f =>
                   E (COLUMN (flatten o f))
                 | NESTING f =>
                   E (NESTING (flatten o f)))
   in
      fun choice {wide, narrow} =
          E (CHOICE {wide = flatten wide, narrow = narrow})
      fun group doc =
          choice {wide = doc, narrow = doc}
   end

   val softline = group line
   val softbreak = group linebreak

   local
      fun mk m (l, r) = l <^> m <^> r
   in
      val op <+>  = mk space
      val op <$>  = mk line
      val op </>  = mk softline
      val op <$$> = mk linebreak
      val op <//> = mk softbreak
   end

   local
      fun mk bop xs =
          case rev xs of
             [] => empty
           | x::xs =>
             foldl bop x xs
   in
      val hsep    = mk op <+>
      val vsep    = mk op <$>
      val fillSep = mk op </>
      val hcat    = mk op <^>
      val vcat    = mk op <$$>
      val fillCat = mk op <//>
   end

   val sep = group o vsep
   val cat = group o vcat

   fun enclose (l, r) d = l <^> d <^> r
   val squotes  = enclose (Sq.mk squote)
   val dquotes  = enclose (Sq.mk dquote)
   val parens   = enclose parens
   val angles   = enclose angles
   val braces   = enclose braces
   val brackets = enclose brackets

   fun fold f s maxCols doc = let
      datatype t' =
         NIL
       | PRINT of String.t * t
       | LINEFEED of Int.t * t
      withtype t = t' Lazy.t

      fun layout s doc =
          case F doc of
             NIL => s
           | PRINT (str, doc) =>
             layout (f (STRING str, s)) doc
           | LINEFEED (cols, doc) =>
             layout (f (NEWLINE cols, s)) doc

      fun fits usedCols doc =
          NONE = maxCols orelse
          usedCols <= valOf maxCols andalso
          case F doc of
             NIL => true
           | LINEFEED _ => true
           | PRINT (str, doc) =>
             fits (usedCols + size str) doc

      fun best usedCols work =
          L (fn () =>
                case work of
                   [] => E NIL
                 | (nestCols, doc)::rest =>
                   case F doc of
                      EMPTY =>
                      best usedCols rest
                    | JOIN (lhs, rhs) =>
                      best usedCols ((nestCols, lhs)::
                                     (nestCols, rhs)::rest)
                    | NEST (cols, doc) =>
                      best usedCols ((nestCols + cols, doc)::rest)
                    | TEXT str =>
                      E (PRINT (str, best (usedCols + size str) rest))
                    | LINE _ =>
                      E (LINEFEED (nestCols, best nestCols rest))
                    | CHOICE {wide, narrow} => let
                      val wide = best usedCols ((nestCols, wide)::rest)
                   in
                      if fits usedCols wide then
                         wide
                      else
                         best usedCols ((nestCols, narrow)::rest)
                   end
                    | COLUMN f =>
                      best usedCols ((nestCols, f usedCols)::rest)
                    | NESTING f =>
                      best usedCols ((nestCols, f nestCols)::rest))
   in
      layout s (best 0 [(0, doc)])
   end

   fun app e = fold (e o #1) ()

   fun pretty n d =
       concat o rev |< fold (fn (STRING s, ss) => s::ss
                              | (NEWLINE n, ss) =>
                                spaces n::"\n"::ss) [] n d

   local
      val join =
          fn [] => empty
           | (_, d)::xs =>
             group d <^> hcat (map (group o uncurry nest o
                                    Pair.map (id, line <\ op <^>)) xs)
   in
      val str =
          join o
          map (Pair.map (SS.size,
                         fillSep o
                         map (txt' o SS.string) o
                         SS.fields C.isSpace) o
               SS.splitl C.isSpace) o
          SS.fields (#"\n" <\ op =) o
          SS.dropl C.isSpace o
          SS.full
   end

   fun println os n d =
       (app (fn STRING s => TextIO.output (os, s)
              | NEWLINE n =>
                (TextIO.output1 (os, #"\n")
               ; repeat (fn () => TextIO.output1 (os, #" ")) n ()))
            n d
      ; TextIO.output1 (os, #"\n")
      ; TextIO.flushOut os)
end
