structure SQL :> SQL =
   struct
      type column = Prim.column
      type db = Query.pool Ring.t
      datatype storage = datatype Prim.storage
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Error = Prim.Error
      
      structure Query = Query
      structure Function = Function
      
      val version = Prim.version
      
      fun columns q = Prim.columns (Query.peek q)
      fun columnsMeta q = Prim.meta (Query.peek q)
      
      fun getDB dbl = 
         case Ring.get dbl of { db, query=_, available=_, used=_ } => 
         db
      
      fun openDB file = 
         Ring.new { db = Prim.openDB file,
                    query = "database",
                    available = ref [],
                    used = ref 0 }
      
      val closeDB = Prim.closeDB o getDB
      
      datatype 'v stop = STOP | CONTINUE of 'v
      
      fun iterStop q i =
         let
            val ok = ref true
            val (pq, oF) = Query.alloc (q, i)
            fun stop () = (
               Query.release (q, pq);
               ok := false)
         in
            fn STOP => (stop (); NONE)
             | (CONTINUE ()) =>
                  if not (!ok) then NONE else
                  if Prim.step pq then SOME (oF pq) else (stop (); NONE)
         end
      
      fun mapStop f q i =
         let
            val (pq, oF) = Query.alloc (q, i)
            fun stop l = (
               Query.release (q, pq);
               Vector.fromList (List.rev l))
            
            fun helper l =
               if Prim.step pq
               then case f (oF pq) of
                       STOP => stop l
                     | CONTINUE r => helper (r :: l)
               else stop l
         in
            helper []
         end
      
      fun appStop f q i =
         let
            val (pq, oF) = Query.alloc (q, i)
            fun stop () = Query.release (q, pq)
            
            fun helper () =
               if Prim.step pq
               then case f (oF pq) of
                       STOP => stop ()
                     | CONTINUE () => helper ()
               else stop ()
         in
            helper ()
         end
      
      fun map f = mapStop (CONTINUE o f)
      fun app f = appStop (CONTINUE o f)
      fun iter q i =
         let
            val step = iterStop q i
         in
            fn () => step (CONTINUE ())
         end
      
      fun table q = map (fn x  => x)  q
      fun exec  q = app (fn () => ()) q
      
      local
         open Query
      in
         fun simpleTable (db, qs) =
            let
               val Q = prepare db qs oAS $
            in
               table Q () before close Q
            end
         
         fun simpleExec (db, qs) =
            let
               val Q = prepare db qs $
            in
               exec Q () before close Q
            end
      end
      
      fun registerFunction (db, s, (f, i)) = 
         Prim.createFunction (getDB db, s, f, i)
         
      fun registerAggregate (db, s, (a, i)) = 
         Prim.createAggregate (getDB db, s, a, i)
      
      fun registerCollation (db, s, c) = 
         Prim.createCollation (getDB db, s, c)
      
      structure SQLite = 
         struct
            val lastInsertRowId = Prim.lastInsertRowid o getDB
            val changes = Prim.changes o getDB
            val totalChanges = Prim.totalChanges o getDB
            val transactionActive = not o Prim.getAutocommit o getDB
            
            datatype access = datatype Prim.access
            datatype request = datatype Prim.request
            fun setAuthorizer (db, f) = Prim.setAuthorizer (getDB db, f)
         end
   end
