(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Glob :> sig
   type t

   structure Infix : sig
      val eps : t

      val C : Char.t -> t
      val S : String.t -> t

      val `* : t
      val `? : t

      val ^` : t BinOp.t
   end

   structure Format : sig
      type t = {esc : Char.t Option.t,
                one : Char.t,
                any : Char.t,
                isDelim : Char.t UnPr.t}
      val def : t
   end

   val scan : Format.t -> (Char.t, 's) Reader.t -> (t, 's) Reader.t
   val fromString : String.t -> t Option.t
   val toString : t -> String.t

   val matchStream : t -> (Char.t, 's) Reader.t -> 's UnPr.t
   val matchString : t -> String.t UnPr.t
end = struct
   datatype e = WILD of Int.t * Bool.t | LIT of Char.t List.t
   type t = e List.t * e

   structure Infix = struct
      val eps = ([], WILD (0, false))
      fun C c = ([], LIT [c])
      fun Cs [] = eps | Cs cs = ([], LIT cs)
      fun S s = Cs (explode s)
      val `* = ([], WILD (0, true))
      val `? = ([], WILD (1, false))

      fun wild (sl, ml) (sr, mr) = WILD (sl + sr, ml orelse mr)
      fun lit ll lr = LIT (ll @ lr)

      val op ^` =
       fn ((ls, WILD wl), ([],      WILD wr)) => (ls, wild wl wr)
        | ((ls,  LIT ll), ([],       LIT lr)) => (ls, lit ll lr)
        | ((ls, WILD wl), (WILD wr::rs, rms)) => (ls @ wild wl wr :: rs, rms)
        | ((ls,  LIT ll), (LIT lr ::rs, rms)) => (ls @ lit ll lr :: rs, rms)
        | ((ls,     lms), (rs,          rms)) => (ls @ lms :: rs, rms)
   end

   structure Format = struct
      type t = {esc : Char.t Option.t, one : Char.t, any : Char.t,
                isDelim : Char.t UnPr.t}
      val def = {esc = SOME #"\\", one = #"?", any = #"*",
                 isDelim = const false} : t
   end

   fun scan {esc, one, any, isDelim} get = let
      open Infix
      fun finish t cs s = SOME (t ^` Cs (rev cs), s)
      fun nonEsc t cs s =
          case get s of
             NONE => finish t cs s
           | SOME (c, s) =>
             if      isDelim c    then finish t cs s
             else if one = c      then nonEsc (t ^` Cs (rev cs) ^` `?) [] s
             else if any = c      then nonEsc (t ^` Cs (rev cs) ^` `*) [] s
             else if esc = SOME c then gotEsc t cs s
             else                      nonEsc t (c::cs) s
      and gotEsc t cs s =
          case get s of
             NONE => NONE
           | SOME (c, s) => nonEsc t (c::cs) s
   in
      nonEsc eps []
   end

   val fromString = StringCvt.scanString (scan Format.def)

   fun toString (es, e) = let
      fun to rs =
       fn LIT cs =>
          List.revAppend
             (List.concatMap
                 (fn #"?" => [#"\\", #"?"]
                   | #"*" => [#"\\", #"*"]
                   | #"\\" => [#"\\", #"\\"]
                   | c => [c])
                 cs,
              rs)
        | WILD (n, m) =>
          List.tabulate (n, const #"?") @ (if m then #"*"::rs else rs)
   in
      implode (rev (to (foldl (fn (e, rs) => to rs e) [] es) e))
   end

   fun matchStream (es, e) = let
      val es = es @ [e]
   in
      fn get => let
            fun next s k =
                case get s of
                   NONE => false
                 | SOME ? => k ?
            fun lp s =
             fn [] => isNone (get s)
              | WILD (n, m)::es => let
                   fun more s = lp s es orelse next s (fn (_, s) => more s)
                   fun skip s = fn 0 => if m then more s else lp s es
                                 | n => next s (fn (_, s) => skip s (n-1))
                in
                   skip s n
                end
              | LIT l::es => let
                   fun match s =
                    fn [] => lp s es
                     | c::cs => next s (fn (c', s) => c = c' andalso match s cs)
                in
                   match s l
                end
         in
            flip lp es
         end
   end

   fun matchString t = matchStream t Substring.getc o Substring.full
end
