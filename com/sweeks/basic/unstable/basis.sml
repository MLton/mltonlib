structure Basis = struct
   structure Array = Array
   structure ArraySlice = ArraySlice
   structure Bool = Bool
   structure Byte = Byte
   structure Char = Char
   structure CommandLine = CommandLine
   structure Date = Date
(*   structure General = General *)
   structure GenericSock = GenericSock
   structure IEEEReal = IEEEReal
   structure INetSock = INetSock
   structure Int = Int
   structure Int16 = Int16
   structure Int32 = Int32
   structure Int64 = Int64
   structure Int8 = Int8
   structure IntInf = IntInf
   structure LargeInt = LargeInt
   structure LargeReal = LargeReal
   structure LargeWord = LargeWord
(*   structure List = List *)
   structure MLton = MLton
   structure NetHostDB = NetHostDB
   structure NetProtDB = NetProtDB
   structure NetServDB = NetServDB
(*   structure Option = Option *)
   structure OS = OS
   structure PackRealBig = PackRealBig
   structure PackRealLittle = PackRealLittle
   structure PackReal32Big = PackReal32Big
   structure PackReal32Little = PackReal32Little
   structure PackReal64Big = PackReal64Big
   structure PackReal64Little = PackReal64Little
   structure PackWord32Big = PackWord32Big
   structure PackWord32Little = PackWord32Little
   structure Position = Position
   structure Posix = Posix
   structure Real = Real
   structure Real32 = Real32
   structure Real64 = Real64
   structure Socket = Socket
   structure String = String
   structure StringCvt = StringCvt
   structure SysWord = SysWord
   structure TextIO = TextIO
   structure Time = Time
   structure UnixSock = UnixSock
   structure Unsafe = Unsafe
   structure Vector = Vector
   structure VectorSlice = VectorSlice
   structure Word = Word
   structure Word16 = Word16
   structure Word32 = Word32
   structure Word64 = Word64
   structure Word8 = Word8

   type 'a array = 'a array
   datatype bool = datatype bool
   type char = char
   type exn = exn
   type int = int
   datatype list = datatype list
   datatype option = datatype option
   datatype order = datatype order
   datatype ref = datatype ref
   type string = String.string
   type 'a vector = 'a vector
   type unit = unit
   type word = word

   exception Empty = Empty
   exception Fail = Fail
   exception Option = Option
   exception Size = Size
   exception Subscript = Subscript

   val ! = !
   val op := = op :=
   val op o = op o
   val ignore = ignore
   val not = not
   val vector = vector

end

open Basis

