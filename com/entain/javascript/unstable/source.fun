(* Copyright (C) 2006 Entain, Inc.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor Source (S: SOURCE_STRUCTS): SOURCE =
struct

open S
   
datatype t = T of {file: File.t ref,
                   lineNum: int ref,
                   lineStart: int ref}

fun getPos (T {file, lineNum, lineStart, ...}, n) =
   SourcePos.make {column = n - !lineStart,
                   file = !file,
                   line = !lineNum}
                
fun lineStart (s as T {lineStart, ...}) = getPos (s, !lineStart)

fun lineDirective (T {file, lineNum, lineStart},
                   f,
                   {lineNum = n, lineStart = s}) =
   (Option.app (f, fn f => file := f)
    ; lineNum := n
    ; lineStart := s)
                      
fun new file = T {file = ref file,
                  lineNum = ref 1,
                  lineStart = ref 1}

fun newline (T {lineStart, lineNum, ...}, n) =
   (Int.inc lineNum
    ; lineStart := n)
   
end
