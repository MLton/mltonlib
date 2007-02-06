structure SQL :> SQL =
   struct
      type db = Prim.db
      type ('a, 'b) query = Prim.query * ('a -> 'b)
      type column = Prim.column
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Fail  = Prim.Fail
      datatype storage = datatype Prim.storage
      
      val openDB  = Prim.openDB
      val closeDB = Prim.closeDB
      
      fun close (q, _) = Prim.finalize q
      fun meta  (q, _) = Prim.meta q
      
      fun step f (q, exec) =
         if Prim.step q 
         then SOME (exec f)
         else (Prim.reset q; NONE)
      
      fun map f (q, exec) =
         let
            fun helper l =
               if Prim.step q
               then helper (exec f :: l)
               else (Prim.reset q; Vector.fromList (List.rev l))
         in
            helper []
         end
      
      structure Template = Template
   end
