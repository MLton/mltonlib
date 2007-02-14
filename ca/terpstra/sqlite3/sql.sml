structure SQL :> SQL =
   struct
      type column = Prim.column
      type db = Prim.db
      datatype storage = datatype Prim.storage
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Error = Prim.Error
      
      structure Query = Query
      
      val version = Prim.version
      
      fun columns (q, _, _) = Prim.meta q
      
      val openDB  = Prim.openDB
      val closeDB = Prim.closeDB
      
      datatype 'v stop = STOP | CONTINUE of 'v
      
      fun iterStop (q, iF, oF) i =
         let
            val () = iF (q, i)
            val ok = ref true
            
            fun stop () = (
               Prim.reset q;
               Prim.clearbindings q;
               ok := false)
         in
            fn STOP => (stop (); NONE)
             | (CONTINUE ()) =>
                  if not (!ok) then NONE else
                  if Prim.step q then SOME (oF q) else (stop (); NONE)
         end
      
      fun mapStop f (q, iF, oF) i =
         let
            val () = iF (q, i)
            
            fun stop l = (
               Prim.reset q;
               Prim.clearbindings q;
               Vector.fromList (List.rev l))
            
            fun helper l =
               if Prim.step q
               then case f (oF q) of
                       STOP => stop l
                     | CONTINUE r => helper (r :: l)
               else stop l
         in
            helper []
         end
      
      fun appStop f (q, iF, oF) i =
         let
            val () = iF (q, i)
            
            fun stop () = (
               Prim.reset q;
               Prim.clearbindings q)
            
            fun helper () =
               if Prim.step q
               then case f (oF q) of
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
   end
