signature SQL =
   sig
      type db
      type 'a query
      type column = { name: string }
      
      exception Retry of string
      exception Abort of string
      exception Fail  of string
      
      val openDB: string -> db
      val closeDB: db -> unit
      
      val close: 'a query -> unit
      val meta:  'a query -> column vector
      
      val step: 'a query -> 'a option
      val map:  ('a -> 'b) -> 'a query -> 'b vector
      val app:  ('a -> unit) -> 'a query -> unit
      
      (* convenience functions *)
      val pull: 'a query -> 'a vector
      val exec: unit query -> unit
      
      datatype storage = INTEGER of Int64.int
                       | REAL of real
                       | STRING of string
                       | BLOB of Word8Vector.vector
                       | NULL
      
      (* You should ignore the type information here. It's confusing and useless.
       * Use this structure as follows:
       * local
       *   open SQL.Template
       * in
       *   val T1 = query "select (a, b) from table 1where x="iI" and y="iS";" oS oR $
       *   val T2 = query "insert into table2 values (4, 6);" $
       * end
       * ...
       * val Q1 = T1 (db & 6 & "sdfs")
       * val Q2 = T2 db
       * 
       * val () = SQL.app (fn (x & y) => ...) Q1
       * val () = SQL.exec Q2
       *)
      structure Template :
         sig
            type ('i, 'o, 'x, 'y) acc
            type ('v, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output = (('i, 'o, 'v, 'x) acc, ('i, 'x, 'y, ('x, 'y) pair) acc, 'a, 'b, 'c) Fold.step0
            type ('v, 'i, 'o, 'x, 'y, 'a, 'b, 'c) input = (string, ('i, 'o, 'x, 'y) acc, (('i, 'v) pair, 'o, 'x, 'y) acc, 'a, 'b, 'c) Fold.step1
            
            val query: string -> ((db, unit, 'a, 'a) acc, ('i, 'o, 'x, 'y) acc, 'i -> 'o query, 'z) Fold.t
            val $ : 'a * ('a -> 'b) -> 'b
            
            (* Convert the next column to the desired type *)
            val oB: (Word8Vector.vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oR: (real,               'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oI: (int,                'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oZ: (Int64.int,          'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oS: (string,             'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oX: (storage,            'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            
            (* Convert all the columns to the desired type in a vector *)
            val oAB: (Word8Vector.vector vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oAR: (real               vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oAI: (int                vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oAZ: (Int64.int          vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oAS: (string             vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            val oAX: (storage            vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output
            
            (* Use a variable of the named type in the SQL statement *)
            val iB: (Word8Vector.vector, 'i, 'o, 'x, 'y, 'a, 'b, 'c) input
            val iR: (real,               'i, 'o, 'x, 'y, 'a, 'b, 'c) input
            val iI: (int,                'i, 'o, 'x, 'y, 'a, 'b, 'c) input
            val iZ: (Int64.int,          'i, 'o, 'x, 'y, 'a, 'b, 'c) input
            val iS: (string,             'i, 'o, 'x, 'y, 'a, 'b, 'c) input
            val iX: (storage,            'i, 'o, 'x, 'y, 'a, 'b, 'c) input
         end
   end
