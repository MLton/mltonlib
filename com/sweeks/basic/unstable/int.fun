functor Int (Int:
             sig
                include BASIS_INT
             end): INT = struct

   type t = Int.int

   local
      open Int
   in
      val op + = op +
      val op - = op -
      val op * = op *
      val op < = op <
      val op <= = op <=
      val op > = op >
      val op >= = op >=
      val op div = op div
      val op mod = op mod
      val quot = quot
      val rem = rem
   end

   val == = op =

   val compare = Order.ofBasis o Int.compare
      
   val zero = Int.fromInt 0

   val one = Int.fromInt 1

   val toWord = Basis.Word.fromLargeInt o Int.toLarge

   fun fromToBy (start, stop, by) =
      #1 (Seq.unfold
          (start,
           if by > zero then
              (fn i => if i >= stop then None else Some (i, i + by))
           else if by < zero then
              (fn i => let
                 val i = i + by
              in
                 if i < stop then
                    None
                 else
                    Some (i, i)
              end)
                else
                   die "Int.fromToBy 0"))

   fun fromTo (start, stop) = fromToBy (start, stop, one)

   fun geu (i, j) = Word.>= (toWord i, toWord j)

   fun toStringRadix (i, r) = Int.fmt (Radix.toBasis r) i

   fun toString i = toStringRadix (i, Radix.dec)

   fun fold (start, stop, b, f) =
      Util.recur ((start, b), fn ((i, b), loop) =>
                  if i >= stop then b else loop (i + one, f (i, b)))

   fun foldDown (start, stop, b, f) =
      Util.recur ((start, b), fn ((i, b), loop) =>
                  if i < stop then
                     b
                  else let
                     val i = i - one
                  in
                     loop (i, f (i, b))
                  end)

   fun for (start, stop, f) = fold (start, stop, (), f o #1)

   fun scanner r = Scanner.ofBasis (Int.scan (Radix.toBasis r))

   fun ofStringRadix (s, r) = Scanner.scanString (scanner r, s)

   fun ofString s = ofStringRadix (s, Radix.dec)

end
