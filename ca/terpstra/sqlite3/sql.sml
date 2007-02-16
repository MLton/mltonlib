structure SQL :> SQL =
   struct
      type column = Prim.column
      type db = Prim.db
      datatype storage = datatype Prim.storage
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Error = Prim.Error
      
      structure Query = Query
      structure Function = Function
      
      val version = Prim.version
      
      fun columns q = Prim.columns (Query.peek q)
      fun columnsMeta q = Prim.meta (Query.peek q)
      
      val openDB  = Prim.openDB
      val closeDB = Prim.closeDB
      
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
      
      fun registerFunction  (db, s, (f, i)) = Prim.createFunction (db, s, f, i)
      fun registerAggregate (db, s, (a, i)) = Prim.createAggregate(db, s, a, i)
      val registerCollation = Prim.createCollation
      
      structure SQLite = 
         struct
            val lastInsertRowId = Prim.lastInsertRowid
            val changes = Prim.changes
            val totalChanges = Prim.totalChanges
            val transactionActive = not o Prim.getAutocommit
            
            datatype access = datatype Prim.access
            datatype request = datatype Prim.request
            val setAuthorizer = Prim.setAuthorizer
         end
   end
