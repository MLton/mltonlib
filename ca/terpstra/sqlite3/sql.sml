structure SQL :> SQL =
   struct
      type db = Prim.db
      type 'a query = Prim.query * (Prim.query -> 'a)
      type column = Prim.column
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Fail  = Prim.Fail
      datatype storage = datatype Prim.storage
      
      val openDB  = Prim.openDB
      val closeDB = Prim.closeDB
      
      fun close (q, _) = Prim.finalize q
      fun meta  (q, _) = Prim.meta q
      
      fun step (q, oF) =
         if Prim.step q 
         then SOME (oF q)
         else (Prim.reset q; NONE)
      
      fun map f (q, oF) =
         let
            fun helper l =
               if Prim.step q
               then helper (f (oF q) :: l)
               else (Prim.reset q; Vector.fromList (List.rev l))
         in
            helper []
         end
      
      fun app f (q, oF) =
         let
            fun helper () =
               if Prim.step q
               then (f (oF q); helper ())
               else Prim.reset q
         in
            helper ()
         end
      
      fun pull q = map (fn x => x) q
      fun exec q = app (fn () => ()) q
      
      structure Template = Template
   end
