signature PRIM =
   sig
      type db
      type query
      type value
      type context
      
      exception Retry of string (* retriable error; busy/locked/etc *)
      exception Abort of string (* transaction aborted *)
      exception Error of string (* database corrupt; close it *)
      
      (* a side-benefit of this as a string is that it forces sqlite3 to be linked *)
      val version: string
      
      (* All of these methods can raise an exception: *)
      
      val openDB: string -> db
      val closeDB: db -> unit
      
      val prepare:  db * string -> query
      val finalize: query -> unit
      val reset:    query -> unit
      val step:     query -> bool
      val clearbindings: query -> unit
      
      val query_string: query -> string
      
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
      val bindN: query * int -> unit
      val bindS: query * int * string -> unit
      val bindX: query * int * storage -> unit
      
      val cols: query -> int
      val fetchB: query * int -> Word8Vector.vector
      val fetchR: query * int -> real
      val fetchI: query * int -> int
      val fetchZ: query * int -> Int64.int
      val fetchN: query * int -> unit
      val fetchS: query * int -> string
      val fetchX: query * int -> storage
      
      (* Every output column has a name.
       * Depending on compile options of sqlite3, you might have more meta-data.
       * We comment out the sections that must be enabled at sqlite3 compile-time.
       *)
      type column = { name: string }
      (*
                      origin: { table:  string,
                                db:     string,
                                decl:   string,
                                schema: string }
                              option } *)
      val meta: query -> column vector
      
      (* User defined methods *)
      val valueB: value -> Word8Vector.vector
      val valueR: value -> real
      val valueI: value -> int
      val valueZ: value -> Int64.int
      val valueN: value -> unit
      val valueS: value -> string
      val valueX: value -> storage
      
      val resultB: context * Word8Vector.vector -> unit
      val resultR: context * real -> unit
      val resultI: context * int -> unit
      val resultZ: context * Int64.int -> unit
      val resultN: context -> unit
      val resultS: context * string -> unit
      val resultX: context * storage -> unit
      
      val createFunction: db * string * (context * value vector -> unit) * int -> unit
(*
      val createCollation: db * string * (string * string -> order) option -> unit
      val createAggregate: db * string * ((context * value vector -> unit) *
                                          (context -> unit)) option -> unit
*)
   end
