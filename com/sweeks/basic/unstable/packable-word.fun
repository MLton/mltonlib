functor PackableWord (structure PackBig: BASIS_PACK_WORD
                      structure PackLittle: BASIS_PACK_WORD
                      structure Word: BASIS_WORD): PACKABLE_WORD = struct

   structure Word = Word (structure Word = Word)

   open Word

   fun switch (e, (b, l)) =
      case e of
         Endian.Big => b
       | Endian.Little => l

   local
      fun make fs (x, y, e) = ofLarge ((switch (e, fs)) (x, y))
   in
      val subArr = make (PackBig.subArr, PackLittle.subArr)
      val subVec = make (PackBig.subVec, PackLittle.subVec)
   end

   fun update (a, i, w, e) =
      (switch (e, (PackBig.update, PackLittle.update))) (a, i, toLarge w)

end
