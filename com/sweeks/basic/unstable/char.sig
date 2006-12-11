structure Word8 = struct
   open Word8
   type t = word
end

signature CHAR = sig

   include ORDERED

   val isAscii: t -> Bool.t
   (**
    * isAscii c = 0 <= toInt c < 128
    *)
   val isAlpha: t -> Bool.t
   (**
    * isAlpha c = isLower c orelse isUpper c
    *)
   val isAlphaNum: t -> Bool.t
   (**
    * isAlphaNum c = isAlpha c orelse isDigit c
    *)
   val isCntrl: t -> Bool.t
   (**
    * isCntrl c = not (isPrint c)
    *)
   val isDigit: t -> Bool.t
   (**
    * isDigit c is true iff c is in [0, 1, 2, ..., 9].
    *)
   val isGraph: t -> Bool.t
   (**
    * isGraph c = isPrint c andalso not (isSpace c)
    *)
   val isHexDigit: t -> Bool.t
   (**
    * isHexDigit c = isDigit c orelse c in [a, b, c, d, e, f, A, B, C, D, E, F].
    *)
   val isLower: t -> Bool.t
   (**
    * isLower c returns true iff c is a lowercase letter.
    *)
   val isPrint: t -> Bool.t
   (**
    * isPrint c returns true iff c is a printable character.
    *)
   val isSpace: t -> Bool.t
   (**
    * isSpace c returns true iff c is a whitespace character (tab, carriage
    * return, newline, vertical tab, form feed).
    *)
   val isPunct: t -> Bool.t
   (**
    * isPunct c = isGraph c andalso not (isAlphaNum c)
    *)
   val isUpper: t -> Bool.t
   (**
    * isUpper c returns true iff c is an uppercase letter.
    *)
   val ofInt: Int.t -> t
   (**
    * ofInt i returns the c such that toInt c = i.
    *)
   val toInt: t -> Int.t
   (**
    * toInt c returns the integer code of the character c.
    *)
   val toLower: t -> t
   (**
    * toLower c returns the lowercase letter corresponding to c, if c is a
    * letter, otherwise returns c.
    *)
   val toUpper: t -> t
   (**
    * toUpper c returns the uppercase letter corresponding to c, if c is a
    * letter, otherwise returns c.
    *)
   val toWord8: t -> Word8.t
   (**
    * returns an 8-bit word holding the code for the character c. 
    *)

end
