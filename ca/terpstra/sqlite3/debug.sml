functor PrimDebug(P : PRIM) :> PRIM =
   struct
      open P
      
      fun wrap (f, s) x = (print (s ^ "\n"); f x)
      
      fun openDB f = wrap (P.openDB,  "openDB: " ^ f) f
      val closeDB  = wrap (P.closeDB, "closeDB")
      
      val prepare = fn (d, q) => wrap (P.prepare, "prepare: " ^ q) (d, q)
      val finalize = wrap (P.finalize, "finalize")
      val reset    = wrap (P.reset, "reset")
      val step     = wrap (P.step, "step") 
      val clearbindings = wrap (P.clearbindings, "clearbindings")
      
      fun bindings q = 
        let val () = print "bindings: "
            val r = P.bindings q
            val () = print (Int.toString r ^ "\n")
        in r end
      
      fun bindWrap (s, p, f) (q, i, x) = (
         print (s ^ " " ^ Int.toString i ^ ": " ^ f x ^ "\n"); 
         p (q, i, x))
      val bindB = bindWrap ("bindB", P.bindB, Int.toString o Word8Vector.length)
      val bindR = bindWrap ("bindR", P.bindR, Real.toString)
      val bindI = bindWrap ("bindI", P.bindI, Int.toString)
      val bindZ = bindWrap ("bindZ", P.bindZ, Int64.toString)
      val bindN = bindWrap ("bindN", P.bindN, fn () => "NULL")
      val bindS = bindWrap ("bindS", P.bindS, fn x => x)
      fun bindX (q, i, INTEGER z) = (print "bindX: "; bindZ (q, i, z))
        | bindX (q, i, REAL r)    = (print "bindX: "; bindR (q, i, r))
        | bindX (q, i, STRING s)  = (print "bindX: "; bindS (q, i, s))
        | bindX (q, i, BLOB b)    = (print "bindX: "; bindB (q, i, b))
        | bindX (q, i, NULL)      = (print "bindX: "; bindN (q, i, ()))
      
      fun cols q = 
        let val () = print "cols: "
            val r = P.cols q
            val () = print (Int.toString r ^ "\n")
        in r end
        
      fun fetchWrap (s, p, f) (q, i) = 
         let val () = print (s ^ " " ^ Int.toString i ^ ": ")
             val r = p (q, i)
             val () = print (f r ^ "\n")
         in r end
      val fetchB = fetchWrap ("fetchB", P.fetchB, Int.toString o Word8Vector.length)
      val fetchR = fetchWrap ("fetchR", P.fetchR, Real.toString)
      val fetchI = fetchWrap ("fetchI", P.fetchI, Int.toString)
      val fetchZ = fetchWrap ("fetchZ", P.fetchZ, Int64.toString)
      val fetchN = fetchWrap ("fetchN", P.fetchN, fn () => "NULL")
      val fetchS = fetchWrap ("fetchS", P.fetchS, fn x => x)
      val fetchX = fetchWrap ("fetchX", P.fetchX, 
                        fn (INTEGER z) => Int64.toString z
                         | (REAL r)    => Real.toString r
                         | (STRING s)  => s
                         | (BLOB b)    => Int.toString (Word8Vector.length b)
                         | NULL        => "(NULL)")
      
      val meta = wrap (P.meta, "meta")
      val columns = wrap (P.columns, "columns")
      
      fun resultWrap (s, p, f) (c, x) = (
         print (s ^ ": " ^ f x ^ "\n"); 
         p (c, x))
      val resultB = resultWrap ("resultB", P.resultB, Int.toString o Word8Vector.length)
      val resultR = resultWrap ("resultR", P.resultR, Real.toString)
      val resultI = resultWrap ("resultI", P.resultI, Int.toString)
      val resultZ = resultWrap ("resultZ", P.resultZ, Int64.toString)
      val resultN = resultWrap ("resultN", P.resultN, fn () => "NULL")
      val resultS = resultWrap ("resultS", P.resultS, fn x => x)
      fun resultX (c, INTEGER z) = (print "resultX: "; resultZ (c, z))
        | resultX (c, REAL r)    = (print "resultX: "; resultR (c, r))
        | resultX (c, STRING s)  = (print "resultX: "; resultS (c, s))
        | resultX (c, BLOB b)    = (print "resultX: "; resultB (c, b))
        | resultX (c, NULL)      = (print "resultX: "; resultN (c, ()))
      
      fun valueWrap (s, p, f) v = 
         let val () = print (s ^ ": ")
             val r = p v
             val () = print (f r ^ "\n")
         in r end
      val valueB = valueWrap ("valueB", P.valueB, Int.toString o Word8Vector.length)
      val valueR = valueWrap ("valueR", P.valueR, Real.toString)
      val valueI = valueWrap ("valueI", P.valueI, Int.toString)
      val valueZ = valueWrap ("valueZ", P.valueZ, Int64.toString)
      val valueN = valueWrap ("valueN", P.valueN, fn () => "NULL")
      val valueS = valueWrap ("valueS", P.valueS, fn x => x)
      val valueX = valueWrap ("valueX", P.valueX, 
                        fn (INTEGER z) => Int64.toString z
                         | (REAL r)    => Real.toString r
                         | (STRING s)  => s
                         | (BLOB b)    => Int.toString (Word8Vector.length b)
                         | NULL        => "(NULL)")
      
      fun createFunction (db, s, f, n) = (
         print ("createFunction: " ^ s ^ " with " ^ Int.toString n ^ " args.\n");
         P.createFunction (db, s, fn x => (print (s ^ " invoked\n"); f x), n))
      
      fun createCollation (db, s, f) = (
         print ("createCollation: " ^ s ^ ".\n");
         P.createCollation (db, s, fn x => (print (s ^ " invoked\n"); f x)))
      
      fun doit s f () =
         let
            val () = print (s ^ "-gen invoked.\n")
            val { step=Pstep, final=Pfinal } = f ()
            fun step (c, v) = (print (s ^ "-step invoked with " ^ 
                                      Int.toString (Vector.length v) ^ "args.\n");
                               Pstep (c, v))
            fun final c = (print (s ^ "-final invoked.\n"); Pfinal c)
         in
            { step = step, final = final }
         end
      
      fun createAggregate (db, s, f, n) = (
         print ("createAggregate: " ^ s ^ " with " ^ Int.toString n ^ " args.\n");
         P.createAggregate (db, s, doit s f, n))
      
      val lastInsertRowid = wrap (P.lastInsertRowid, "lastInsertRowid")
      val changes = wrap (P.changes, "changes")
      val totalChanges = wrap (P.totalChanges, "totalChanges")
      val getAutocommit = wrap (P.getAutocommit, "getAutocommit")
      
      (* be more clever *)
      val setAuthorizer = wrap (P.setAuthorizer, "setAuthorizer")
   end

structure Prim = PrimDebug(Prim)
