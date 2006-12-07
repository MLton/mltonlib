structure Radix: sig

   include RADIX

   val toBasis: t -> StringCvt.radix
      
end = struct

   datatype t = Bin | Dec | Hex | Oct

   val bin = Bin
   val dec = Dec
   val hex = Hex
   val oct = Oct

   val toString =
      fn Bin => "Bin"
       | Dec => "Dec"
       | Hex => "Hex"
       | Oct => "Oct"

   val toBasis =
      fn Bin => StringCvt.BIN
       | Dec => StringCvt.DEC
       | Hex => StringCvt.HEX
       | Oct => StringCvt.OCT

end
