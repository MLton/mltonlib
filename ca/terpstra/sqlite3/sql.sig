signature SQL =
   sig
      type db
      type ('a, 'b) query
      type column = { name: string }
      
      exception Retry of string
      exception Abort of string
      exception Fail  of string
      
      val openDB: string -> db
      val closeDB: db -> unit
      
      val close: ('a, 'b) query -> unit
      val meta:  ('a, 'b) query -> column vector
      
      val step: 'a -> ('a, 'b) query -> 'b option
      val map:  'a -> ('a, 'b) query -> 'b vector
      
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
       *   val T1 : SQL.db -> int -> string -> (string -> real -> out, out) query
       *          = query "select (a, b) from table where x="iI" and y="iS";" oS oR $
       * end
       *)
      structure Template :
         sig
            type ('o, 'of, 'i, 'r) acc
            
            val query: string -> (('r,  'of, ('of, 'r) query, 'r) acc, ('of, 'of, 'i, 'r) acc, db -> 'i, 'y, 'z) Foldr.t
            val $ : 'a * ('a -> 'b) -> 'b
            
            (* Convert all the columns to the desired type in a vector *)
            val oAB: (('o, 'of, 'i, 'r) acc, (Word8Vector.vector vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oAR: (('o, 'of, 'i, 'r) acc, (real               vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oAI: (('o, 'of, 'i, 'r) acc, (int                vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oAZ: (('o, 'of, 'i, 'r) acc, (Int64.int          vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oAS: (('o, 'of, 'i, 'r) acc, (string             vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oAX: (('o, 'of, 'i, 'r) acc, (storage            vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            
            (* Convert the next column to the desired type *)
            val oB: (('o, 'of, 'i, 'r) acc, (Word8Vector.vector -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oR: (('o, 'of, 'i, 'r) acc, (real               -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oI: (('o, 'of, 'i, 'r) acc, (int                -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oZ: (('o, 'of, 'i, 'r) acc, (Int64.int          -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oS: (('o, 'of, 'i, 'r) acc, (string             -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            val oX: (('o, 'of, 'i, 'r) acc, (storage            -> 'o, 'of, 'i, 'r) acc, 'x, 'y, 'z) Foldr.step0
            
            (* Use a variable of the named type in the SQL statement *)
            val iB: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, Word8Vector.vector -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
            val iR: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, real               -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
            val iI: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, int                -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
            val iZ: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, Int64.int          -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
            val iS: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, string             -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
            val iX: (string, ('o, 'of, 'i, 'r) acc, ('o, 'of, storage            -> 'i, 'r) acc, 'x, 'y, 'z) Foldr.step1
         end
   end
