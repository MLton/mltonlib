(*
** 2007 February 18
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** $Id$
*)
structure SQL :> SQL =
   struct
      type column = Prim.column
      type db = { ring: Query.pool Ring.t,
                  free: Prim.query list ref,
                  hooks: Prim.hook list ref,
                  auth: Prim.hook option ref }
      datatype storage = datatype Prim.storage
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Error = Prim.Error
      
      structure Query = Query
      structure Function = Function
      
      val version = Prim.version
      
      fun getDB { free, ring, hooks=_, auth=_ } = 
         case Ring.get ring of { db, query=_, available=_, used } => 
         if !used = ~1 then raise Error "Database closed" else 
         ( Query.cleanup free; db)
      
      fun openDB file = {
         ring = Ring.new { db = Prim.openDB file,
                           query = "database", 
                           available = ref [],
                           used = ref 0 },
         free = ref [],
         hooks = ref [],
         auth = ref NONE }
      
      fun closeDB { ring, free, hooks, auth } = 
         let
            val { db, query=_, available=_, used } = Ring.get ring 
            val () = if !used = ~1 then raise Error "Database closed" else ()
            
            fun notInUse { db=_, query=_, available=_, used } = !used = 0
            
            val exn = ref NONE
            fun reraise NONE = ()
              | reraise (SOME x) = raise x
            
            fun forceClose q = Prim.finalize q handle x => exn := SOME x
            fun close { db=_, query=_, available, used } = (
               List.app forceClose (!available before available := []);
               used := ~1)
            
            fun main () =
               if Ring.fold (fn (l, a) => notInUse l andalso a) true ring
               then (Ring.app close ring; 
                     List.app forceClose (!free before free := []);
                     reraise (!exn); 
                     Prim.closeDB db;
                     List.app Prim.unhook (!hooks before hooks := []);
                     Option.app Prim.unhook (!auth before auth := NONE))
               else raise Error "Database in use"
         in
            MLton.Thread.atomically (fn () => main ())
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
      
      fun registerFunction (db as { ring=_, free=_, hooks, auth=_ }, s, (f, i)) = 
         hooks := Prim.createFunction (getDB db, s, f, i) :: !hooks
         
      fun registerAggregate (db as { ring=_, free=_, hooks, auth=_ }, s, (a, i)) = 
         hooks := Prim.createAggregate (getDB db, s, a, i) :: !hooks
      
      fun registerCollation (db as { ring=_, free=_, hooks, auth=_ }, s, c) = 
         hooks := Prim.createCollation (getDB db, s, c) :: !hooks
      
      structure SQLite = 
         struct
            val lastInsertRowId = Prim.lastInsertRowid o getDB
            val changes = Prim.changes o getDB
            val totalChanges = Prim.totalChanges o getDB
            val transactionActive = not o Prim.getAutocommit o getDB
            
            fun preparedQueries { free=_, ring, hooks=_, auth=_ } =
               MLton.Thread.atomically
                  (fn () => Ring.fold (fn (_, x) => x + 1) ~1 ring)
            fun registeredFunctions { free=_, ring=_, hooks, auth=_ } =
               List.length (!hooks)
            
            datatype access = datatype Prim.access
            datatype request = datatype Prim.request
            
            fun setAuthorizer (dbh as { ring=_, free=_, hooks=_, auth }, f) = 
               let
                  val db = getDB dbh
                  fun unset h = (Prim.unsetAuthorizer db; Prim.unhook h; auth := NONE)
                  fun set f = auth := SOME (Prim.setAuthorizer (db, f))
               in
                  Option.app unset (!auth);
                  Option.app set f
               end
         end
   end
