local
   open Basis
in
   structure LargeWord = Word (structure Word = LargeWord)
   structure SysWord = Word (structure Word = SysWord)
   structure Word = Word (structure Word = Word)
   structure Word8 = Word (structure Word = Word8)
   structure Word16 = Word (structure Word = Word16)
   structure Word32 = PackableWord (structure PackBig = PackWord32Big
                                    structure PackLittle = PackWord32Little
                                    structure Word = Word32)
   structure Word64 = Word (structure Word = Word64)
end
