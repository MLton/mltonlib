signature SQL =
   sig
      type db
      type column = { name: string }
      
      (* For unconverted type values *)
      datatype storage = INTEGER of Int64.int
                       | REAL of real
                       | STRING of string
                       | BLOB of Word8Vector.vector
                       | NULL
      
      (* You should ignore the type information here. It's confusing & useless.
       * Use this structure as follows:
       * local
       *   open SQL.Query
       * in
       *   val Q1 = prepare db "select (a, b) from table 1where x="iI" and y="iS";" oS oR $
       *   val Q2 = prepare db "insert into table2 values (4, 6);" $
       * end
       * ...
       * val () = SQL.app (fn (x & y) => ...) Q1 (1 & "arg2")
       * val () = SQL.exec Q2 ()
       * val () = SQL.Query.close Q1
       * val () = SQL.Query.close Q2
       *)
      structure Query :
         sig
            type ('i, 'o, 'w, 'x, 'y, 'z) acc
            type ('v, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output = 
               (('i, 'o, 'v, 'p,            'a, 'b) acc, 
                ('i, 'p, 'q, ('p, 'q) pair, 'a, 'b) acc, 
                'x, 'y, 'z) Fold.step0
            type ('v, 'i, 'o, 'j, 'k, 'a, 'b, 'x, 'y, 'z) input = 
               (string, ('i, 'o, 'a, 'b, 'j, 'v) acc, 
                        ('j, 'o, 'a, 'b, ('j, 'k) pair, 'k) acc, 
                        'x, 'y, 'z) Fold.step1
            
            type ('i, 'o) t
            val prepare: db -> string -> ((unit, unit, 'a, 'a, 'b, 'b) acc,
                                          ('i,   'o,   'c, 'd, 'e, 'f) acc, 
                                          ('i, 'o) t, 'g) Fold.t
            val $ : 'a * ('a -> 'b) -> 'b
            
            (* For every 'query' you must eventually run this: *)
            val close: ('i, 'o) t -> unit
            
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
      
      exception Retry of string
      exception Abort of string
      exception Error of string
      
      (* The version of SQLite3 bound *)
      val version: string
      
      (* Open and close databases -- all queries must be closed *)
      val openDB: string -> db
      val closeDB: db -> unit
      
      (* Meta-data about the columns in the output *)
      val columns: ('i, 'o) Query.t -> column vector
      
      (* Transform a query into an iterator *)
      val iter: ('i, 'o) Query.t -> 'i -> unit -> 'o option
      
      (* Run a function on each output row from a query *)
      val map: ('o -> 'v) -> ('i, 'o) Query.t -> 'i -> 'v vector
      val app: ('o -> unit) -> ('i, 'o) Query.t -> 'i -> unit
      
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
   end
