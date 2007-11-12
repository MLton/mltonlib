(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure OS = struct
   open BasisOS

   structure FileSys = struct
      open FileSys

      fun foldDir f s path =
          case openDir path
           of ds =>
              Exn.after (fn () => let
                               fun lp s =
                                   case readDir ds
                                    of NONE => s
                                     | SOME n => lp (f (n, s))
                            in
                               lp s
                            end,
                         fn () => closeDir ds)

      val listDir = foldDir op :: []
   end
end
