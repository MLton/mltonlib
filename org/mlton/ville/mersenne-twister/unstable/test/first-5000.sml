val () = let
  open MersenneTwister
  val mt = new 0w1234567890
  fun loop 5000 = ()
    | loop i = (
      print (Word32.toString (rand mt));
      if (i + 1) mod 8 = 0 then
        print "\n"
      else
        print " ";
      loop (i + 1) )
  val _ = loop 0
in () end
