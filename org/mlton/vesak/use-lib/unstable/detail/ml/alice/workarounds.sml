(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure OS = struct
   open OS

   structure FileSys = struct
      open FileSys

      local
         fun try (th, fv, fe) =
             ((case th () of v => fn () => fv v) handle e => fn () => fe e) ()

         fun after (th, ef) =
             try (th, fn x => (ef () ; x), fn e => (ef () ; raise e))
      in
         (* WARNING: Totally ignores links. *)
         fun access (p, _) =
             try (fn () => TextIO.openIn p,
                  fn s => (TextIO.closeIn s ; true),
                  fn _ => false)

         (* WARNING: Tests always for only read access only. *)
         fun fullPath p =
             case getDir ()
              of cwd =>
                 after (fn () =>
                           if isDir p
                           then (chDir p ; getDir ())
                           else case Path.splitDirFile p
                                 of {dir, file} =>
                                    (if "" <> dir then chDir dir else ()
                                   ; Path.joinDirFile {dir = dir, file = file}),
                        fn () => chDir cwd)
      end
   end
end
