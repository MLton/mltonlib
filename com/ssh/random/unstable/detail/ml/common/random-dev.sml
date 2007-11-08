(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RandomDev : RANDOM_DEV = struct
   val cnt = ref 0w0
   fun seed () =
       SOME (Word.xorb
                (Word.fromLargeInt
                    (Time.toMicroseconds (Time.now ()) mod
                     Word.toLargeInt Word.maxValue),
                 !cnt)
             before cnt := !cnt + 0w1)
   val useed = seed
end
