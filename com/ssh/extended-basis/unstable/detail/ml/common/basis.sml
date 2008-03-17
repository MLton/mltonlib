(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * We rebind all Basis signatures, structures and functors that we are
 * interested in to make it easier to extend them.  This is mainly to make
 * it easier to write the Compilation Manager (CM) files for SML/NJ.
 *)

signature BASIS_ARRAY = ARRAY
(*signature BASIS_ARRAY2 = ARRAY2*)
signature BASIS_ARRAY_SLICE = ARRAY_SLICE
signature BASIS_BIN_IO = BIN_IO
(*signature BASIS_BIT_FLAGS = BIT_FLAGS*)
signature BASIS_BOOL = BOOL
signature BASIS_BYTE = BYTE
signature BASIS_CHAR = CHAR
signature BASIS_COMMAND_LINE = COMMAND_LINE
signature BASIS_DATE = DATE
signature BASIS_GENERAL = GENERAL
(*signature BASIS_GENERIC_SOCK = GENERIC_SOCK*)
(*signature BASIS_IEEE_REAL = IEEE_REAL*)
signature BASIS_IMPERATIVE_IO = IMPERATIVE_IO
(*signature BASIS_INET_SOCK = INET_SOCK*)
signature BASIS_INTEGER = INTEGER
signature BASIS_INT_INF = INT_INF
signature BASIS_IO = IO
signature BASIS_LIST = LIST
signature BASIS_LIST_PAIR = LIST_PAIR
signature BASIS_MATH = MATH
signature BASIS_MONO_ARRAY = MONO_ARRAY
(*signature BASIS_MONO_ARRAY2 = MONO_ARRAY2*)
signature BASIS_MONO_ARRAY_SLICE = MONO_ARRAY_SLICE
signature BASIS_MONO_VECTOR = MONO_VECTOR
signature BASIS_MONO_VECTOR_SLICE = MONO_VECTOR_SLICE
(*signature BASIS_NET_HOST_DB = NET_HOST_DB*)
(*signature BASIS_NET_PROT_DB = NET_PROT_DB*)
(*signature BASIS_NET_SERV_DB = NET_SERV_DB*)
signature BASIS_OPTION = OPTION
signature BASIS_OS = OS
signature BASIS_OS_FILE_SYS = OS_FILE_SYS
signature BASIS_OS_IO = OS_IO
signature BASIS_OS_PATH = OS_PATH
signature BASIS_OS_PROCESS = OS_PROCESS
signature BASIS_PACK_REAL = PACK_REAL
signature BASIS_PACK_WORD = PACK_WORD
(*signature BASIS_POSIX = POSIX*)
(*signature BASIS_POSIX_ERROR = POSIX_ERROR*)
(*signature BASIS_POSIX_FILE_SYS = POSIX_FILE_SYS*)
(*signature BASIS_POSIX_IO = POSIX_IO*)
(*signature BASIS_POSIX_PROCESS = POSIX_PROCESS*)
(*signature BASIS_POSIX_PROC_ENV = POSIX_PROC_ENV*)
(*signature BASIS_POSIX_SIGNAL = POSIX_SIGNAL*)
(*signature BASIS_POSIX_SYS_DB = POSIX_SYS_DB*)
(*signature BASIS_POSIX_TTY = POSIX_TTY*)
(*signature BASIS_PRIM_IO = PRIM_IO*)
signature BASIS_REAL = REAL
(*signature BASIS_SOCKET = SOCKET*)
signature BASIS_STREAM_IO = STREAM_IO
signature BASIS_STRING = STRING
signature BASIS_STRING_CVT = STRING_CVT
signature BASIS_SUBSTRING = SUBSTRING
signature BASIS_TEXT = TEXT
signature BASIS_TEXT_IO = TEXT_IO
signature BASIS_TEXT_STREAM_IO = TEXT_STREAM_IO
signature BASIS_TIME = TIME
signature BASIS_TIMER = TIMER
(*signature BASIS_UNIX = UNIX*)
(*signature BASIS_UNIX_SOCK = UNIX_SOCK*)
signature BASIS_VECTOR = VECTOR
signature BASIS_VECTOR_SLICE = VECTOR_SLICE
signature BASIS_WORD = WORD
(* signature BASIS_WINDOWS = WINDOWS *)

structure BasisArray = Array
(*structure BasisArray2 = Array2*)
structure BasisArraySlice = ArraySlice
structure BasisBinIO = BinIO
structure BasisBool = Bool
structure BasisByte = Byte
structure BasisChar = Char
structure BasisCharArray = CharArray
structure BasisCharArraySlice = CharArraySlice
structure BasisCharVector = CharVector
structure BasisCharVectorSlice = CharVectorSlice
structure BasisCommandLine = CommandLine
structure BasisDate = Date
structure BasisGeneral = General
(*structure BasisGenericSock = GenericSock*)
structure BasisIEEEReal = IEEEReal
(*structure BasisINetSock = INetSock*)
structure BasisIO = IO
structure BasisInt = Int
structure BasisLargeInt = LargeInt
structure BasisLargeReal = LargeReal
structure BasisLargeWord = LargeWord
structure BasisList = List
structure BasisListPair = ListPair
(*structure BasisNetHostDB = NetHostDB*)
(*structure BasisNetProtDB = NetProtDB*)
(*structure BasisNetServDB = NetServDB*)
structure BasisOS = OS
structure BasisOption = Option
structure BasisPosition = Position
(*structure BasisPosix = Posix*)
structure BasisReal = Real
(*structure BasisSocket = Socket*)
structure BasisString = String
structure BasisStringCvt = StringCvt
structure BasisSubstring = Substring
structure BasisText = Text
structure BasisTextIO = TextIO
structure BasisTime = Time
structure BasisTimer = Timer
(*structure BasisUnix = Unix*)
(*structure BasisUnixSock = UnixSock*)
structure BasisVector = Vector
structure BasisVectorSlice = VectorSlice
structure BasisWord = Word
structure BasisWord8 = Word8
structure BasisWord8Array = Word8Array
structure BasisWord8ArraySlice = Word8ArraySlice
structure BasisWord8Vector = Word8Vector
structure BasisWord8VectorSlice = Word8VectorSlice
(* structure BasisWindows = Windows *)

(* signature BASIS_IMPERATIVE_IO_ARG = sig *)
(*    structure StreamIO : STREAM_IO *)
(*    structure Vector : MONO_VECTOR *)
(*    structure Array : MONO_ARRAY *)
(*    sharing type StreamIO.elem *)
(*               = Vector.elem = Array.elem *)
(*    sharing type StreamIO.vector *)
(*               = Vector.vector *)
(*               = Array.vector *)
(* end *)

(* functor BasisImperativeIO (Arg : BASIS_IMPERATIVE_IO_ARG) : IMPERATIVE_IO *)
(*   = ImperativeIO (Arg) *)

(* signature BASIS_PRIM_IO_ARG = sig *)
(*    structure Vector : MONO_VECTOR *)
(*    structure VectorSlice : MONO_VECTOR_SLICE *)
(*    structure Array : MONO_ARRAY *)
(*    structure ArraySlice : MONO_ARRAY_SLICE *)
(*    sharing type Vector.elem *)
(*               = VectorSlice.elem *)
(*               = Array.elem *)
(*               = ArraySlice.elem *)
(*    sharing type Vector.vector *)
(*               = VectorSlice.vector *)
(*               = Array.vector *)
(*               = ArraySlice.vector *)
(*    sharing type VectorSlice.slice *)
(*               = ArraySlice.vector_slice *)
(*    sharing type Array.array *)
(*               = ArraySlice.array *)
(*    val someElem : Vector.elem *)
(*    eqtype pos *)
(*    val compare : pos * pos -> order *)
(* end *)

(* functor BasisPrimIO (Arg : BASIS_PRIM_IO_ARG) : PRIM_IO = PrimIO (Arg) *)

(* signature BASIS_STREAM_IO_ARG = sig *)
(*    structure PrimIO : PRIM_IO *)
(*    structure Vector : MONO_VECTOR *)
(*    structure Array : MONO_ARRAY *)
(*    sharing type PrimIO.elem *)
(*               = Vector.elem *)
(*               = Array.elem *)
(*    sharing type PrimIO.vector *)
(*               = Vector.vector *)
(*               = Array.vector *)
(*    sharing type PrimIO.array *)
(*               = Array.array *)
(*    val someElem : PrimIO.elem *)
(* end *)

(* functor BasisStreamIO (Arg : BASIS_STREAM_IO_ARG) : STREAM_IO = StreamIO (Arg) *)
