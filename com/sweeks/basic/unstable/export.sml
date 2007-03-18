(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Export:>
   EXPORT
   where type 'a Array.t = 'a Array.t
   where type 'a Array.elem = 'a Array.elem
   where type ('a, 'b) Array.unfold = ('a, 'b) Array.unfold
   where type ('a, 'b) Array.unfoldR = ('a, 'b) Array.unfoldR
   where type 'a ArraySlice.elem = 'a ArraySlice.elem
   where type Char.t = Char.t
   where type In.t = In.t
   where type Int.t = Int.t
   where type Int8.t = Int8.t
   where type Int16.t = Int16.t
   where type Int32.t = Int32.t
   where type Int64.t = Int64.t
   where type IntInf.t = IntInf.t
   where type LargeInt.t = LargeInt.t
   where type LargeReal.t = LargeReal.t
   where type LargeWord.t = LargeWord.t
   where type 'a List.elem = 'a List.elem
   where type ('a, 'b) List.unfold = ('a, 'b) List.unfold
   where type ('a, 'b) List.unfoldR = ('a, 'b) List.unfoldR
   where type 'a Option.t = 'a Option.t
   where type Out.t = Out.t
   where type Radix.t = Radix.t
   where type Real.t = Real.t
   where type Real32.t = Real32.t
   where type Real64.t = Real64.t
   where type 'a Scanner.t = 'a Scanner.t
   where type 'a Seq.t = 'a Seq.t
   where type 'a Seq.elem = 'a Seq.elem
   where type ('a, 'b) Seq.unfold = ('a, 'b) Seq.unfold
   where type ('a, 'b) Seq.unfoldR = ('a, 'b) Seq.unfoldR
   where type String.t = String.t
   where type ('a, 'b) String.unfold = ('a, 'b) String.unfold
   where type ('a, 'b) String.unfoldR = ('a, 'b) String.unfoldR
   where type Substring.t = Substring.t
   where type SysWord.t = SysWord.t
   where type Time.t = Time.t
   where type Unit.t = Unit.t
   where type 'a Vector.t = 'a Vector.t
   where type 'a Vector.elem = 'a Vector.elem
   where type ('a, 'b) Vector.unfold = ('a, 'b) Vector.unfold
   where type ('a, 'b) Vector.unfoldR = ('a, 'b) Vector.unfoldR
   where type 'a VectorSlice.t = 'a VectorSlice.t
   where type 'a VectorSlice.elem = 'a VectorSlice.elem
   where type Word.t = Word.t
   where type Word8.t = Word8.t
   where type Word16.t = Word16.t
   where type Word32.t = Word32.t
   where type Word64.t = Word64.t
   = struct

   structure Array = Array
   structure ArraySlice = ArraySlice
   structure Bool = Bool
   structure Char = Char
   structure CommandLine = CommandLine
   structure Date = Date
   structure Dir = Dir
   structure Endian = Endian
   structure Exn = Exn
   structure File = File
   structure In = In
   structure Int = Int
   structure Int8 = Int8
   structure Int16 = Int16
   structure Int32 = Int32
   structure Int64 = Int64
   structure IntInf = IntInf
   structure IoDesc = IoDesc
   structure LargeInt = LargeInt
   structure LargeReal = LargeReal
   structure LargeWord = LargeWord
   structure Lazy = Lazy
   structure List = List
   structure Net = Net
   structure Option = Option
   structure Order = Order
   structure Out = Out
   structure Path = Path
   structure Poll = Poll
   structure Posix = Posix
   structure Process = Process
   structure Radix = Radix
   structure Real = Real
   structure Real32 = Real32
   structure Real64 = Real64
   structure Ref = Ref
   structure Scanner = Scanner
   structure Seq = Seq
   structure String = String
   structure Substring = Substring
   structure SysError = SysError
   structure SysWord = SysWord
   structure Time = Time
   structure Unit = Unit
   structure Vector = Vector
   structure VectorSlice = VectorSlice
   structure Word = Word
   structure Word8 = Word8
   structure Word16 = Word16
   structure Word32 = Word32
   structure Word64 = Word64

   val op @ = op @
   val ! = !
   val op := = op :=
   val concat = concat
   val die = die
   val finally = finally
   val id = id
   val ignore = ignore
   val lazy = lazy
   val not = not
   val op o = op o
   val pass = pass
   val print = print
   val recur = recur
   val valOf = valOf

end
