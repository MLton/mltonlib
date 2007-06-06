(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Prettier :> PRETTIER = struct
   (* <-- SML/NJ workaround *)
   open Basic Fn Lazy
   infix  4 <\
   infixr 2 |<
   val map = List.map
   (* SML/NJ workaround --> *)

   structure C = Char and S = String and SS = Substring

   infixr 7 <^> <+>
   infixr 6 <$> <$$> </> <//>

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
          if S.all C.isPrint str then ()
          else fail "unprintable characters given to Prettier.txt"
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
