structure Char: CHAR where type t = char = struct

   type t = char

   local
      open Char
   in
      val op < = op <
      val op <= = op <=
      val op > = op >
      val op >= = op >=
      val isAscii = isAscii
      val isAlpha = isAlpha
      val isAlphaNum = isAlphaNum
      val isCntrl = isCntrl
      val isDigit = isDigit
      val isGraph = isGraph
      val isHexDigit = isHexDigit
      val isLower = isLower
      val isPrint = isPrint
      val isSpace = isSpace
      val isPunct = isPunct
      val isUpper = isUpper
      val ofInt = chr
      val toInt = ord
      val toLower = toLower
      val toUpper = toUpper
   end

   val == = op =

   val compare = Order.ofBasis o Char.compare

   val toWord8 = Byte.charToByte
      
end
