structure Time: TIME = struct

   open Time
      
   type t = time

   val == = op =

   val compare = Order.ofBasis o compare

   val zero = zeroTime

   val ofSeconds = fromSeconds
   val ofMilliseconds = fromMilliseconds
   val ofMicroseconds = fromMicroseconds
   val ofNanoseconds = fromNanoseconds

   fun format (t, {fractionalDigits = n}) = fmt n t

   val scanner = Scanner.ofBasis scan

   fun ofString s = Scanner.scanString (scanner, s)

end
