(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature EXPORT = sig

   structure Array: ARRAY
   structure ArraySlice: ARRAY_SLICE
   structure Bool: BOOL
   structure Char: CHAR
   structure CommandLine: COMMAND_LINE
   structure Date: DATE
   structure Dir: DIR
   structure Endian: ENDIAN
   structure Exn: EXN
   structure File: FILE
   structure In: IN
   structure Int: INT
   structure Int8: INT
   structure Int16: INT
   structure Int32: INT
   structure Int64: INT
   structure IntInf: INT_INF
   structure IoDesc: IO_DESC
   structure LargeInt: INT
   structure LargeReal: REAL
   structure LargeWord: WORD
   structure Lazy: LAZY
   structure List: LIST
   structure Net: NET
   structure Option: OPTION
   structure Order: ORDER
   structure Out: OUT
   structure Path: PATH
   structure Poll: POLL
   structure Posix: POSIX
   structure Process: PROCESS
   structure Radix: RADIX
   structure Real: PACKABLE_REAL
   structure Real32: PACKABLE_REAL
   structure Real64: PACKABLE_REAL
   structure Ref: REF
   structure Scanner: SCANNER
   structure Seq: SEQ
   structure String: STRING
   structure Substring: SUBSTRING
   structure SysError: SYS_ERROR
   structure SysWord: WORD
   structure Time: TIME
   structure Unit: UNIT
   structure Vector: VECTOR
   structure VectorSlice: VECTOR_SLICE
   structure Word: WORD
   structure Word8: WORD8
   structure Word16: WORD
   structure Word32: PACKABLE_WORD
   structure Word64: WORD

   sharing type ArraySlice.base = Array.t
   sharing type VectorSlice.base = Vector.t

   val ! : 'a Ref.t -> 'a
   val := : 'a Ref.t * 'a -> Unit.t
   val @ : 'a List.t * 'a List.t -> 'a List.t
   val concat: String.t Seq.t -> String.t
   val die: String.t -> 'a
   val finally: 'a thunk * Unit.t thunk -> 'a
   val id: 'a -> 'a
   val ignore: 'a -> Unit.t
   val lazy: 'a thunk -> 'a thunk
   val not: Bool.t -> Bool.t
   val o: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   val pass: 'a -> ('a -> 'b) -> 'b
   val print: String.t -> Unit.t
   val recur: 'a * ('a * ('a -> 'b) -> 'b) -> 'b
   val valOf: 'a Option.t -> 'a
end
