(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature STRING = sig
   include STRING
   val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
end
