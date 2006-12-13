(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure File: FILE = struct

   type t = String.t

   val openIn = TextIO.openIn
   val openOut = TextIO.openOut

   local
      fun make (openIt, close) (file, f) = let
         val s = openIt file
      in
         finally (fn () => f s, fn () => close s)
      end
   in
      val withIn = fn ? => make (openIn, In.close) ?
      val withOut = fn ? => make (openOut, Out.close) ?
   end

   local
      open Basis.OS.FileSys
   in
      val canAccess = access
      val id = fileId
      val modTime = modTime
      val rename = rename
      val remove = remove
      val setTime = fn (f, t) => setTime (f, SOME t)
      val size = fileSize
   end

   val temp = MLton.TextIO.mkstemps

   structure AccessMode = struct
      datatype t = datatype OS.FileSys.access_mode

      val exec = A_EXEC
      val read = A_READ
      val write = A_WRITE
   end

   structure Id = struct
      open OS.FileSys

      type t = file_id

      val == = op =
      val compare = Order.ofBasis o compare
      val hash = hash
   end

end
