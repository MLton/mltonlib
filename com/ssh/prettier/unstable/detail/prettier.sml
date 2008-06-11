(* Copyright (C) 2008 Vesa Karvonen
 * Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Prettier :> PRETTIER = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 2 |<
   infix >>=
   (* SML/NJ workaround --> *)

   structure C = Char and S = String and SS = Substring

   infixr 7 <^> <+>
   infixr 6 <$> <$$> </> <//>

   datatype t =
      EMPTY
    | LAZY of t Lazy.t
    | LINE of Bool.t
    | JOIN of t Sq.t
    | NEST of Int.t * t
    | TEXT of String.t
    | CHOICE of {wide : t, narrow : t}
    | COLUMN of Int.t -> t
    | NESTING of Int.t -> t

   val lazy = LAZY o delay

   val empty = EMPTY
   val line = LINE false
   val linebreak = LINE true
   val column = COLUMN
   val nesting = NESTING

   val txt = TEXT
   val chr = TEXT o str

   val parens   as (lparen,   rparen)   = (TEXT "(", TEXT ")")
   val angles   as (langle,   rangle)   = (TEXT "<", TEXT ">")
   val braces   as (lbrace,   rbrace)   = (TEXT "{", TEXT "}")
   val brackets as (lbracket, rbracket) = (TEXT "[", TEXT "]")
   val squote    = TEXT "'"
   val dquote    = TEXT "\""
   val semi      = TEXT ";"
   val colon     = TEXT ":"
   val comma     = TEXT ","
   val space     = TEXT " "
   val dot       = TEXT "."
   val backslash = TEXT "\\"
   val equals    = TEXT "="

   val op <^> = JOIN

   fun punctuate sep =
    fn []    => []
     | d::ds => let
          fun lp rs d1 =
           fn []     => List.revAppend (rs, [d1])
            | d2::ds => lp (d1 <^> sep::rs) d2 ds
       in
          lp [] d ds
       end

   fun nest n = n <\ NEST

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
          lazy (fn () =>
                   case doc
                    of LAZY doc           => flatten (force doc)
                     | EMPTY              => doc
                     | JOIN (lhs, rhs)    => JOIN (flatten lhs, flatten rhs)
                     | NEST (cols, doc)   => NEST (cols, flatten doc)
                     | TEXT _             => doc
                     | LINE b             => if b then empty else space
                     | CHOICE {wide, ...} => wide
                     | COLUMN f           => COLUMN (flatten o f)
                     | NESTING f          => NESTING (flatten o f))
   in
      fun choice {wide, narrow} =
          CHOICE {wide = flatten wide, narrow = narrow}
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
          case rev xs
           of []    => empty
            | x::xs => foldl bop x xs
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

   fun renderer maxCols w doc = let
      open IOSMonad

      datatype t =
         NIL
       | PRINT of String.t * t Lazy.t
       | LINEFEED of Int.t * t Lazy.t

      val rec layout =
       fn NIL                  => return ()
        | PRINT (str, doc)     => w str >>= (fn () => layout (force doc))
        | LINEFEED (cols, doc) => w "\n" >>= (fn () =>
                                  w (spaces cols) >>= (fn () =>
                                  layout (force doc)))

      fun fits usedCols doc =
          isNone maxCols orelse let
             fun lp usedCols doc =
                 usedCols <= valOf maxCols andalso
                 case force doc
                  of NIL              => true
                   | LINEFEED _       => true
                   | PRINT (str, doc) => lp (usedCols + size str) doc
          in
             lp usedCols (eager doc)
          end

      fun best usedCols =
       fn [] => NIL
        | (nestCols, doc)::rest =>
          case doc
           of LAZY doc =>
              best usedCols ((nestCols, force doc)::rest)
            | EMPTY =>
              best usedCols rest
            | JOIN (lhs, rhs) =>
              best usedCols ((nestCols, lhs)::(nestCols, rhs)::rest)
            | NEST (cols, doc) =>
              best usedCols ((nestCols + cols, doc)::rest)
            | TEXT str =>
              PRINT (str, delay (fn () => best (usedCols + size str) rest))
            | LINE _ =>
              LINEFEED (nestCols, delay (fn () => best nestCols rest))
            | CHOICE {wide, narrow} => let
                 val wide = best usedCols ((nestCols, wide)::rest)
              in
                 if fits usedCols wide
                 then wide
                 else best usedCols ((nestCols, narrow)::rest)
              end
            | COLUMN f =>
              best usedCols ((nestCols, f usedCols)::rest)
            | NESTING f =>
              best usedCols ((nestCols, f nestCols)::rest)
   in
      layout (best 0 [(0, doc)])
   end

   fun render maxCols doc =
       concat o rev o #2 |< renderer maxCols (IOSMonad.fromWriter op ::) doc []

   local
      val join =
          fn []         => empty
           | (_, d)::xs =>
             group d <^> hcat (map (group o uncurry nest o
                                    Pair.map (id, line <\ op <^>)) xs)
   in
      val str =
          join o
          map (Pair.map (SS.size,
                         fillSep o
                         map (TEXT o SS.string) o
                         SS.fields C.isSpace) o
               SS.splitl C.isSpace) o
          SS.fields (#"\n" <\ op =) o
          SS.dropl C.isSpace o
          SS.full
   end

   fun output outstream c d =
       ignore (renderer c (IOSMonad.fromPutter TextIO.output) d outstream)

   fun println c d =
       (output TextIO.stdOut c d ; print "\n")
end
