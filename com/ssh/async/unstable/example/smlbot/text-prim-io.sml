(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TextPrimIO = struct
   open TextPrimIO

   structure RD = struct
      datatype t = datatype reader
      local
         fun S s (RD r) = s r
         fun O s r a = valOf (S s r) a
      in
         val name      = S#name
         val chunkSize = S#chunkSize
         val readVec   = O#readVec
         val readArr   = O#readArr
         val readVecNB = O#readVecNB
         val readArrNB = O#readArrNB
         val block     = O#block
         val canInput  = O#canInput
         val avail     = pass () o S#avail
         val getPos    = O#getPos
         val setPos    = O#setPos
         val endPos    = O#endPos
         val verifyPos = O#verifyPos
         val close     = pass () o S#close
         val ioDesc    = valOf o S#ioDesc
      end
   end

   structure WR = struct
      datatype t = datatype writer
      local
         fun S s (WR r) = s r
         fun O s r a = valOf (S s r) a
      in
         val name       = S#name
         val chunkSize  = S#chunkSize
         val writeVec   = O#writeVec
         val writeArr   = O#writeArr
         val writeVecNB = O#writeVecNB
         val writeArrNB = O#writeArrNB
         val block      = O#block
         val canOutput  = O#canOutput
         val getPos     = O#getPos
         val setPos     = O#setPos
         val endPos     = O#endPos
         val verifyPos  = O#verifyPos
         val close      = pass () o S#close
         val ioDesc     = valOf o S#ioDesc
      end
   end
end
