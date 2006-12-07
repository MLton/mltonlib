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
   structure Time: TIME
   structure Vector: VECTOR
   structure VectorSlice: VECTOR_SLICE
   structure Word: WORD
   structure Word8: WORD
   structure Word16: WORD
   structure Word32: PACKABLE_WORD
   structure Word64: WORD

   sharing type ArraySlice.base = Array.t
   sharing type VectorSlice.base = Vector.t

   type 'a array
   datatype bool = datatype Bool.t
   type char = Char.t
   type exn = Exn.t
   type int = Int.t
   datatype list = datatype List.t
   datatype order = datatype order
   datatype option = datatype Option.t
   type string = String.t
   type 'a seq = 'a Seq.t
   type 'a thunk = 'a Thunk.t
   type 'a vector = 'a Vector.t
   type word = Word.t
   type unit = unit

   val @ : 'a list * 'a list -> 'a list
   val concat: string seq -> string
   val die: string -> 'a
   val finally: 'a thunk * unit thunk -> 'a
   val ignore: 'a -> unit
   val lazy: 'a thunk -> 'a thunk
   val not: bool -> bool
   val o: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   val print: string -> unit
   val valOf: 'a option -> 'a

end
