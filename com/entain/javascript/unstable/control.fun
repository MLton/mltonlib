(* Copyright (C) 2006 Entain, Inc.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Control (S: CONTROL_STRUCTS): CONTROL =
struct

open S

local
   open Region
in
   structure SourcePos = SourcePos
end

val acceptMozillaExtensions = ref false

val numErrors: int ref = ref 0

val errorThreshhold: int ref = ref 20

val die = Process.fail

local
   fun msg (kind: string, r: Region.t, msg: Layout.t, extra: Layout.t): unit =
      let
         open Layout
         val p =
            case Region.left r of
               NONE => "<bogus>"
             | SOME p => SourcePos.toString p
         val msg = Layout.toString msg
         val msg =
            Layout.str
            (concat [String.fromChar (Char.toUpper (String.sub (msg, 0))),
                     String.dropPrefix (msg, 1),
                     "."])
         in
            outputl (align [seq [str (concat [kind, ": "]), str p, str "."],
                            indent (align [msg,
                                           indent (extra, 2)],
                                    2)],
                     Out.error)
      end
in
   fun error (r, m, e) =
      let
         val _ = Int.inc numErrors
         val _ = msg ("Error", r, m, e)
      in
         if !numErrors = !errorThreshhold
            then die "compilation aborted: too many errors"
         else ()
      end
end

fun errorStr (r, msg) = error (r, Layout.str msg, Layout.empty)

end

structure SourcePos = SourcePos ()
structure Region = Region (structure SourcePos = SourcePos)
structure Control = Control (structure Region = Region)
