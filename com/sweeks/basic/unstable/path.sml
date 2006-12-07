structure Path: PATH = struct

   structure Arc = struct
      type t = string

      local
         open OS.Path
      in
         val current = currentArc
         val parent = parentArc
      end
   end

   structure Volume = struct
      type t = string

      fun isValid (v, {isAbs}) = OS.Path.validVolume {isAbs = isAbs, vol = v}
   end

   structure Pieces = struct
      datatype t = T of {arcs: Arc.t list,
                         isAbs: bool,
                         volume: Volume.t}

      fun ofBasis {arcs, isAbs, vol} =
         T {arcs = arcs,
            isAbs = isAbs,
            volume = vol}
         
      fun toBasis (T {arcs, isAbs, volume}) =
         {arcs = arcs,
          isAbs = isAbs,
          vol = volume}
   end

   type t = string

   local
      open OS.FileSys
   in
      val full = fullPath
      val isDir = isDir
      val isLink = isLink
      val readLink = readLink
      val real = realPath
   end

   local
      open OS.Path
   in
      val append = concat
      val base = base
      val dir = dir
      val ext = Option.ofBasis o ext
      val file = file
      val getParent = getParent
      val getVolume = getVolume
      val isAbsolute = isAbsolute
      val isCanonical = isCanonical
      val isRelative = isRelative
      val isRoot = isRoot
      val joinBaseExt =
         fn {base, ext} => joinBaseExt {base = base, ext = Option.toBasis ext}
      val joinDirFile = joinDirFile
      val ofPieces = toString o Pieces.toBasis
      val ofUnix = fromUnixPath
      val splitBaseExt = fn p => let
         val {base, ext} = splitBaseExt p
      in
         {base = base, ext = Option.ofBasis ext}
      end
      val splitDirFile = splitDirFile
      val toPieces = Pieces.ofBasis o fromString
      val toUnix = toUnixPath
   end

   fun mkAbsolute (p, {relativeTo = r}) =
      OS.Path.mkAbsolute {path = p, relativeTo = r}

   val mkCanonical = OS.Path.mkCanonical

   fun mkRelative (p, {relativeTo = r}) =
      OS.Path.mkRelative {path = p, relativeTo = r}

end
