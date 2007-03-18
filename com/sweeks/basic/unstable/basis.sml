(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Basis = struct
   structure Array = struct open Array type 'a t = 'a array end
   structure ArraySlice = ArraySlice
   structure Bool = struct open Bool datatype t = datatype bool end
   structure Byte = Byte
   structure Char = struct open Char type t = char end
   structure CommandLine = CommandLine
   structure Date = Date
   structure Exn = struct type t = exn end
(*   structure General = General *)
   structure GenericSock = GenericSock
   structure IEEEReal = IEEEReal
   structure INetSock = INetSock
   structure Int = struct open Int type t = int end
   structure Int8 = Int8
   structure Int16 = Int16
   structure Int32 = Int32
   structure Int64 = Int64
   structure IntInf = IntInf
   structure LargeInt = LargeInt
   structure LargeReal = LargeReal
   structure LargeWord = LargeWord
   structure List = struct open List datatype t = datatype list end
   structure MLton = MLton
   structure NetHostDB = NetHostDB
   structure NetProtDB = NetProtDB
   structure NetServDB = NetServDB
   structure Option = struct open Option datatype t = datatype option end
   structure Order = struct datatype t = datatype order end
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
   structure Ref = struct datatype t = datatype ref end
   structure Socket = Socket
   structure String = struct open String type t = string end
   structure StringCvt = StringCvt
   structure SysWord = SysWord
   structure TextIO = TextIO
   structure Time = Time
   structure Unit = struct type t = unit end
   structure UnixSock = UnixSock
   structure Unsafe = Unsafe
   structure Vector = struct open Vector type 'a t = 'a vector end
   structure VectorSlice = VectorSlice
   structure Word = struct open Word type t = word end
   structure Word8 = Word8
   structure Word16 = Word16
   structure Word32 = Word32
   structure Word64 = Word64

   datatype z = datatype bool
   datatype z = datatype list
   datatype z = datatype option
   datatype z = datatype ref

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

