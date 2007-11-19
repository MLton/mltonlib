(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

load "OS" ;
load "Substring" ;

structure OS = struct
   open OS
   structure Path = struct
      open Path
      val mkRelative = fn {path, relativeTo} => mkRelative (path, relativeTo)
   end
end

structure Substring = struct
   open Substring
   fun full s = extract (s, 0, NONE)
end

structure String = struct
   fun isSuffix suf str = let
      val sufSz = size suf
      val strSz = size str
   in
      sufSz <= strSz andalso
      EQUAL = Substring.compare (Substring.full suf,
                                 Substring.substring (str, strSz-sufSz, sufSz))
   end
   open String
end
