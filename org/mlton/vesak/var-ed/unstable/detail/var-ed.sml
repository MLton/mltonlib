(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure VarEd :> VAR_ED = struct
   datatype tree = VAR of var
                 | GROUP of group
   withtype group = {name : String.t,
                     children : tree List.t Ref.t,
                     refresh : Bool.t Ref.t}
        and var = {name : String.t,
                   read : String.t Effect.t,
                   pretty : Prettier.t Thunk.t,
                   refresh : Bool.t Ref.t}

   fun assertName name =
       if String.length name > 0
          andalso String.all (neg Char.isSpace) name
       then ()
       else fail "Names must not be empty or contain spaces"

   structure Group = struct
      type t = group
      fun newRoot name =
          (assertName name
         ; {name = name, children = ref [], refresh = ref true})
      fun new {parent = {children, refresh, ...} : t, name} =
          case newRoot name
           of result => (push children (GROUP result)
                       ; refresh := true
                       ; result)
   end

   structure Var = struct
      datatype 'a t =
         IN of {cell : 'a Ref.t,
                assert : 'a UnOp.t,
                signal : 'a t Effect.t,
                refresh : Bool.t Ref.t}
      val ! = fn IN {cell, ...} => !cell
      val op := = fn (self as IN {cell, assert, signal, refresh, ...}, value) =>
                     (cell := assert value ; refresh := true ; signal self)
      fun new {group = {children, refresh, ...} : Group.t,
               name, rep, value, assert, signal} = let
         val () = assertName name
         val var = IN {cell = ref (assert value),
                       assert = assert,
                       signal = signal,
                       refresh = refresh}
         val read = Generic.read rep
         val pretty = Generic.pretty rep
      in
         push children
              (VAR {name = name,
                    read = fn s => var := read s,
                    pretty = fn () => pretty (!var),
                    refresh = refresh})
       ; var
      end
   end

   datatype t =
      IN of {root : group,
             current : group Ref.t,
             parents : group List.t Ref.t}

   fun new {name} =
       case Group.newRoot name
        of root => IN {root = root,
                       current = ref root,
                       parents = ref []}

   fun root (IN {root, ...}) = root

   fun update (IN {current, parents, ...})
              {instream, outstream, ansi, columns} = let
      fun print s =
          (TextIO.output (outstream, s)
         ; TextIO.flushOut outstream)

      local
         open Cvt Prettier
      in
         fun pprintln d =
             (output outstream columns (group d) ; print "\n")
         val D = D
         val P = P
         val fillSep = fillSep
         val txt = txt
         val nest = nest 4
         val op <^> = op <^>
         val op <+> = op <+>
         val op <$> = op <$>
         val colon = colon
      end

      fun prompt () = print "> "

      fun maybeRefresh () =
          if !(#refresh (!current))
          then (#refresh (!current) := false
              ; if ansi then print "\027[1J\027[H" else ()
              ; pprintln
                 (fillSep
                   (List.intersperse
                     (txt "->")
                     (rev (txt (#name (!current)) ::
                           map (txt o #name) (!parents)))))
              ; (List.fori (rev (!(#children (!current)))))
                 (fn (i, n) =>
                     pprintln
                      (nest
                        (txt (P#l 2 (D i)) <^> colon <+>
                         (case n
                           of GROUP {name, ...} =>
                              txt name <+> txt ".."
                            | VAR {name, pretty, ...} =>
                              txt name <$> pretty ()))))
              ; prompt ())
          else ()

      fun processInput () =
          case TextIO.canInput (instream, 1)
           of NONE => NONE
            | _ =>
          case TextIO.inputLine instream
           of NONE => NONE
            | SOME ln =>
          case Substring.string
                (Substring.droplr Char.isSpace (Substring.full ln))
           of "" => (#refresh (!current) := true ; NONE)
            | ".." => (Option.app (fn c => current := c) (pop parents)
                     ; #refresh (!current) := true
                     ; NONE)
            | cmd => let
                 val (i, v) =
                     Substring.splitl (neg Char.isSpace) (Substring.full cmd)
              in
                 case case Int.fromString (Substring.string i)
                       of SOME i => let
                             val n = length (!(#children (!current)))
                          in
                             if 0 <= i andalso i < n then SOME (n-i-1) else NONE
                          end
                        | NONE =>
                          Option.map
                           #1
                           (List.findi
                             (fn (_, c) =>
                                 Substring.compare
                                  (i,
                                   Substring.full
                                    (case c
                                      of GROUP {name, ...} => name
                                       | VAR {name, ...} => name)) = EQUAL)
                             (!(#children (!current))))
                  of NONE => (prompt () ; SOME cmd)
                   | SOME i =>
                     case List.sub (!(#children (!current)), i)
                      of GROUP group =>
                         (push parents (!current)
                        ; current := group
                        ; #refresh (!current) := true
                        ; NONE)
                       | VAR {read, ...} =>
                         (read
                           (Substring.string (Substring.dropl Char.isSpace v))
                          handle e => println (Exn.message e)
                        ; prompt ()
                        ; NONE)
              end
   in
      maybeRefresh ()
    ; processInput ()
   end
end
