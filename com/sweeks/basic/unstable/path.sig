(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature PATH = sig

   type t = String.t

   structure Arc: sig
      type t = String.t
         
      val parent: t
      val current: t
   end

   structure Volume: sig
      type t = String.t

      val isValid: t * {isAbs: Bool.t} -> Bool.t
   end

   structure Pieces: sig
      datatype t = T of {arcs: Arc.t List.t,
                         isAbs: Bool.t,
                         volume: Volume.t}
   end

   val append: t * t -> t
   val base: t -> t
   val dir: t -> t
   val ext: t -> String.t Option.t
   val file: t -> File.t
   val full: t -> t
   val getParent: t -> t
   val getVolume: t -> Volume.t
   val isAbsolute: t -> Bool.t
   val isCanonical: t -> Bool.t
   val isDir: t -> Bool.t
   val isLink: t -> Bool.t
   val isRelative: t -> Bool.t
   val isRoot: t -> Bool.t
   val joinBaseExt: {base: t, ext: String.t Option.t} -> t
   val joinDirFile: {dir: String.t, file: String.t} -> String.t
   val mkAbsolute: t * {relativeTo: t} -> t
   val mkCanonical: t -> t
   val mkRelative: t * {relativeTo: t} -> t
   val ofPieces: Pieces.t -> t
   val ofUnix: t -> t
   val readLink: t -> t
   val real: t -> t
   val splitBaseExt: t -> {base: t, ext: String.t Option.t}
   val splitDirFile: String.t -> {dir: String.t, file: String.t}
   val toPieces: t -> Pieces.t
   val toUnix: t -> t

end
