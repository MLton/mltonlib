signature SQL =
   sig
      type db
      type column = { name: string,
                      origin: { table:  string,
                                db:     string,
                                decl:   string,
                                schema: string }
                              option }
      
      exception Retry of string
      exception Abort of string
      exception Error of string
      
      (* For access to the raw untyped entries in SQLite3: *)
      datatype storage = INTEGER of Int64.int
                       | REAL of real
                       | STRING of string
                       | BLOB of Word8Vector.vector
                       | NULL
      
      (* The version of SQLite3 bound *)
      val version: string
      
      (* Open and close databases *)
      val openDB: string -> db
      val closeDB: db -> unit
      
      (* How many prepared queries are there *)
      val preparedQueries: db -> int
      
      (* You should ignore the type information here. It's confusing & useless.
       * Use this structure as follows:
       * local
       *   open SQL.Query
       * in
       *   val Q1 = prepare db "select (a, b) from table where x="iI" and y="iS";" oS oR $
       *   val Q2 = prepare db "insert into table2 values (4, 6);" $
       * end
       * ...
       * val () = SQL.app (fn (x & y) => ...) Q1 (1 & "arg2")
       * val () = SQL.exec Q2 ()
       *)
      structure Query :
         sig
            type ('i, 'o) t
            
            (* don't look at this: *)
            type ('i, 'o, 'w, 'x, 'y, 'z) acc
            type ('v, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output = 
               (('i, 'o, 'v, 'p,            'a, 'b) acc, 
                ('i, 'p, 'q, ('p, 'q) pair, 'a, 'b) acc, 
                'x, 'y, 'z) Fold.step0
            type ('v, 'i, 'o, 'j, 'k, 'a, 'b, 'x, 'y, 'z) input = 
               (string, ('i, 'o, 'a, 'b, 'j, 'v) acc, 
                        ('j, 'o, 'a, 'b, ('j, 'k) pair, 'k) acc, 
                        'x, 'y, 'z) Fold.step1
            
            val prepare: db -> string -> ((unit, unit, 'a, 'a, 'b, 'b) acc,
                                          ('i,   'o,   'c, 'd, 'e, 'f) acc, 
                                          ('i, 'o) t, 'g) Fold.t
            val $ : 'a * ('a -> 'b) -> 'b
            
            (* Convert the next column to the desired type *)
            val oB: (Word8Vector.vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oR: (real,               'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oI: (int,                'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oZ: (Int64.int,          'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oS: (string,             'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oX: (storage,            'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            
            (* Convert all the columns to the desired type in a vector *)
            val oAB: (Word8Vector.vector vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oAR: (real               vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oAI: (int                vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oAZ: (Int64.int          vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oAS: (string             vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            val oAX: (storage            vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output
            
            (* Use a variable of the named type in the SQL statement *)
            val iB: (Word8Vector.vector, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
            val iR: (real,               'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
            val iI: (int,                'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
            val iZ: (Int64.int,          'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
            val iS: (string,             'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
            val iX: (storage,            'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) input
         end
      
      (* Names of the output columns *)
      val columns: ('i, 'o) Query.t -> string vector
      (* Column meta-data requires SQLITE_ENABLE_COLUMN_METADATA compiled *)
      val columnsMeta: ('i, 'o) Query.t -> column vector
      
      (* Run a function on each output row from a query *)
      val map: ('o -> 'v) -> ('i, 'o) Query.t -> 'i -> 'v vector
      val app: ('o -> unit) -> ('i, 'o) Query.t -> 'i -> unit
      
      (* Transform a query into an iterator *)
      val iter: ('i, 'o) Query.t -> 'i -> unit -> 'o option
      
      (* Run a function on each output row, allowing premature completion *)
      datatype 'v stop = STOP | CONTINUE of 'v
      val mapStop: ('o -> 'v stop) -> ('i, 'o) Query.t -> 'i -> 'v vector
      val appStop: ('o -> unit stop) -> ('i, 'o) Query.t -> 'i -> unit
      val iterStop: ('i, 'o) Query.t -> 'i -> unit stop -> 'o option
      
      (* Convenience functions that work with the identity *)
      val table: ('i, 'o) Query.t -> 'i -> 'o vector
      val exec: ('i, unit) Query.t -> 'i -> unit
      
      (* For simple queries you only run once, use: *)
      val simpleTable: db * string -> string vector vector
      val simpleExec: db * string -> unit
      
      (* This is used to create user functions available to SQL.
       * Example usage:
       * local
       *   open SQL.Function
       *   fun concat (a & b) = a ^ b
       *   fun pi () = 3.14159
       *   fun dump v = Vector.app (fn s => print (s ^ "\n")) v
       *
       *   val sum2 = { init = fn () => 0, 
       *                step = fn (i, (j & k)) => i+j+k, 
       *                finish = fn x => x }
       * in
       *   val () = SQL.registerFunction (db, "concat", fnS iS iS $ concat)
       *   val () = SQL.registerFunction (db, "pi", fnR $ pi)
       *   val () = SQL.registerFunction (db, "dump", fnN iAS $ dump)
       *   val () = SQL.registerAggregate (db, "sum2", aggrI iI iI $ sum2)
       * end
       *)
      structure Function:
         sig
            type scalar
            type aggregate
            
            (* The value 'a can safely contain mutable fields *)
            type ('a, 'b, 'c) folder = {
               init: unit -> 'a,
               step: 'a * 'b -> 'a,
               finish: 'a -> 'c
            }
            
            (* don't look at this: *)
            type ('a, 'b, 'c) acc
            type ('v, 'a, 'b, 'c, 'd, 'e) fnX = 
               ((unit, 'a, 'a) acc, ('b, 'c, 'd) acc, ('b -> 'v) -> scalar, 'e) Fold.t 
            type ('v, 'a, 'b, 'c, 'd, 'e, 'f) aggrX = 
               ((unit, 'a, 'a) acc, ('b, 'c, 'd) acc, ('f, 'b, 'v) folder -> aggregate, 'e) Fold.t 
            type ('v, 'a, 'b, 'c, 'd, 'e, 'f) input = 
               (('a, 'v, 'b) acc, ('b, 'c, ('b, 'c) pair) acc, 'd, 'e, 'f) Fold.step0
            type ('v, 'a, 'b, 'c) inputA = 
               ((unit, unit, unit) acc, ('v vector, unit, unit) acc, 'a, 'b, 'c) Fold.step0
            
            (* Return types of the function *)
            val fnB: (Word8Vector.vector, 'a, 'b, 'c, 'd, 'e) fnX
            val fnR: (real,               'a, 'b, 'c, 'd, 'e) fnX
            val fnI: (int,                'a, 'b, 'c, 'd, 'e) fnX
            val fnZ: (Int64.int,          'a, 'b, 'c, 'd, 'e) fnX
            val fnS: (string,             'a, 'b, 'c, 'd, 'e) fnX
            val fnX: (storage,            'a, 'b, 'c, 'd, 'e) fnX
            val fnN: (unit,               'a, 'b, 'c, 'd, 'e) fnX
            
            (* Return types of the aggregate *)
            val aggrB: (Word8Vector.vector, 'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrR: (real,               'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrI: (int,                'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrZ: (Int64.int,          'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrS: (string,             'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrX: (storage,            'a, 'b, 'c, 'd, 'e, 'f) aggrX
            val aggrN: (unit,               'a, 'b, 'c, 'd, 'e, 'f) aggrX
            
            val $ : 'a * ('a -> 'b) -> 'b
            
            (* Input parameters to the function *)
            val iB: (Word8Vector.vector, 'a, 'b, 'c, 'd, 'e, 'f) input
            val iR: (real,               'a, 'b, 'c, 'd, 'e, 'f) input
            val iI: (int,                'a, 'b, 'c, 'd, 'e, 'f) input
            val iZ: (Int64.int,          'a, 'b, 'c, 'd, 'e, 'f) input
            val iS: (string,             'a, 'b, 'c, 'd, 'e, 'f) input
            val iX: (storage,            'a, 'b, 'c, 'd, 'e, 'f) input
            
            (* Variadic functions *)
            val iAB: (Word8Vector.vector, 'a, 'b, 'c) inputA
            val iAR: (real,               'a, 'b, 'c) inputA
            val iAI: (int,                'a, 'b, 'c) inputA
            val iAZ: (Int64.int,          'a, 'b, 'c) inputA
            val iAS: (string,             'a, 'b, 'c) inputA
            val iAX: (storage,            'a, 'b, 'c) inputA
            
            (* get/set auxdata? could be useful *)
         end
      
      (* SQL.Error exceptions in callbacks are propogated ok. Others not. *)
      val registerFunction:  db * string * Function.scalar -> unit
      val registerAggregate: db * string * Function.aggregate -> unit
      val registerCollation: db * string * (string * string -> order) -> unit
      
      (* SQLite specific methods; see its documentation *)
      structure SQLite:
         sig
            val lastInsertRowId: db -> Int64.int
            val changes: db -> int
            val totalChanges: db -> int
            val transactionActive: db -> bool
            
            datatype access = ALLOW | DENY | IGNORE
            datatype request =
               CREATE_INDEX of { index: string, table: string, db: string, temporary: bool }
             | CREATE_TABLE of { table: string, db: string, temporary: bool }
             | CREATE_TRIGGER of { trigger: string, table: string, db: string, temporary: bool }
             | CREATE_VIEW of { view: string, db: string, temporary: bool }
             | DROP_INDEX of { index: string, table: string, db: string, temporary: bool }
             | DROP_TABLE of { table: string, db: string, temporary: bool }
             | DROP_TRIGGER of { trigger: string, table: string, db: string, temporary: bool }
             | DROP_VIEW of { view: string, db: string, temporary: bool }
             | ALTER_TABLE of { db: string, table: string }
             | REINDEX of { index: string, db: string }
             | ANALYZE of { table: string, db: string }
             | INSERT of { table: string, db: string }
             | UPDATE of { table: string, column: string, db: string }
             | DELETE of { table: string, db: string }
             | TRANSACTION of { operation: string }
             | SELECT
             | READ of { table: string, column: string, db: string  }
             | PRAGMA of { pragma: string, arg: string, db: string option }
             | ATTACH of { file: string }
             | DETACH of { db: string }
             | CREATE_VTABLE of { table: string, module: string, db: string }
             | DROP_VTABLE of { table: string, module: string, db: string  }
             | FUNCTION of { function: string }
            val setAuthorizer: db * (request -> access) option -> unit
            
            (* All of these are omitted from the SML binding: *)
            (* fun interrupt: db -> unit *) (* too dangerous to expose IMO *)
            (* trace? who cares -- use debug.sml *)
            (* update hook? who cares -- use a trigger+callback *)
            (* commit/rollback hook? who cares, wrap them in SML *)
            (* SQLite memory/thread management is explicitly NOT exposed *)
            (* extension support omitted as it is experimental *)
         end
   end
