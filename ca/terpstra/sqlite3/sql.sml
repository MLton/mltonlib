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
      
      fun getDB dbl = 
         case Ring.get dbl of { db, query=_, available=_, used } => 
         if !used = ~1 then raise Error "Database closed" else db
      
      fun openDB file = 
         Ring.new { db = Prim.openDB file,
                    query = "database",
                    available = ref [],
                    used = ref 0 }
      
      fun closeDB dbl = 
         let
            val db = getDB dbl (* raises if closed *)
            fun notInUse { db=_, query=_, available=_, used } = !used = 0
            
            val exn = ref NONE
            fun reraise NONE = ()
              | reraise (SOME x) = raise x
            
            fun forceClose q = Prim.finalize q handle x => exn := SOME x
            fun close { db=_, query=_, available, used } = (
               List.app forceClose (!available);
               available := [];
               used := ~1)
         in
            if Ring.fold (fn (l, a) => notInUse l andalso a) true dbl
            then (Ring.app close dbl; reraise (!exn); Prim.closeDB db)
            else raise Error "Database in use"
         end
      
      fun columns q = Query.peek (q, Prim.columns)
      fun columnsMeta q = Query.peek (q, Prim.meta)
      
      datatype 'v stop = STOP | CONTINUE of 'v
      
      fun iterStop q i =
         let
            val ok = ref true
            val (pq, oF) = Query.alloc (q, i)
            fun stop () = (
               Query.release (q, pq);
               ok := false;
               NONE)
         in
            fn STOP => 
                  if not (!ok) then NONE else stop ()
             | (CONTINUE ()) =>
                  if not (!ok) then NONE else
                  if Prim.step pq then SOME (oF pq) else stop ()
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
               table Q ()
            end
         
         fun simpleExec (db, qs) =
            let
               val Q = prepare db qs $
            in
               exec Q ()
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
            
            fun preparedQueries dbl =
               Ring.fold (fn (_, x) => x + 1) ~1 dbl
            
            datatype access = datatype Prim.access
            datatype request = datatype Prim.request
            fun setAuthorizer (db, f) = Prim.setAuthorizer (getDB db, f)
         end
   end
