signature PATH = sig

   type t = string

   structure Arc: sig
      type t = string
         
      val parent: t
      val current: t
   end

   structure Volume: sig
      type t = string

      val isValid: t * {isAbs: bool} -> bool
   end

   structure Pieces: sig
      datatype t = T of {arcs: Arc.t list,
                         isAbs: bool,
                         volume: Volume.t}
   end

   val append: t * t -> t
   val base: t -> t
   val dir: t -> t
   val ext: t -> string option
   val file: t -> File.t
   val full: t -> t
   val getParent: t -> t
   val getVolume: t -> Volume.t
   val isAbsolute: t -> bool
   val isCanonical: t -> bool
   val isDir: t -> bool
   val isLink: t -> bool
   val isRelative: t -> bool
   val isRoot: t -> bool
   val joinBaseExt: {base: t, ext: string option} -> t
   val joinDirFile: {dir: string, file: string} -> string
   val mkAbsolute: t * {relativeTo: t} -> t
   val mkCanonical: t -> t
   val mkRelative: t * {relativeTo: t} -> t
   val ofPieces: Pieces.t -> t
   val ofUnix: t -> t
   val readLink: t -> t
   val real: t -> t
   val splitBaseExt: t -> {base: t, ext: string option}
   val splitDirFile: string -> {dir: string, file: string}
   val toPieces: t -> Pieces.t
   val toUnix: t -> t

end
