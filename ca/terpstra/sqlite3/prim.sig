signature PRIM =
   sig
      type db
      type query
      
      exception Retry of string (* retriable error; busy/locked/etc *)
      exception Abort of string (* transaction aborted *)
      exception Fail  of string (* database corrupt; close it *)
      
      val version: string
      
      (* All of these methods can raise an exception *)
      
      val openDB: string -> db
      val closeDB: db -> unit
      
      val prepare:  db * string -> query
      val finalize: query -> unit
      val step:     query -> unit
      
      datatype storage = INTEGER of Int64.int
                       | REAL of real
                       | STRING of string
                       | BLOB of Word8Vector.vector
                       | NULL
      
      val bindings: query -> int
      val bindB: query * int * Word8Vector.vector -> unit
      val bindR: query * int * real -> unit
      val bindI: query * int * int -> unit
      val bindZ: query * int * Int64.int -> unit
      val bindS: query * int * string -> unit
      val bindX: query * int * storage -> unit
      
      val cols: query -> int
      val fetchB: query * int -> Word8Vector.vector
      val fetchR: query * int -> real
      val fetchI: query * int -> int
      val fetchZ: query * int -> Int64.int
      val fetchS: query * int -> string
      val fetchX: query * int -> storage
      
      val databases: query -> string option vector
      val decltypes: query -> string option vector
      val tables:    query -> string option vector
      val origins:   query -> string option vector
      val names:     query -> string vector
   end
