functor PrimDebug(P : PRIM) :> PRIM =
   struct
      open P
      
      fun wrap (f, s) x = (print (s ^ "\n"); f x)
      
      fun openDB f = wrap (P.openDB,  "openDB: " ^ f) f
      val closeDB  = wrap (P.closeDB, "closeDB")
      
      val prepare = fn (d, q) => wrap (P.prepare, "prepare: " ^ q) (d, q)
      val reset    = wrap (P.reset, "reset")
      val finalize = wrap (P.finalize, "finalize")
      val step     = wrap (P.step, "step") 
      
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
      val bindS = bindWrap ("bindS", P.bindS, fn x => x)
      fun bindN (q, i) = print ("bindN " ^ Int.toString i ^ ": NULL\n")
      fun bindX (q, i, INTEGER z) = (print "bindX: "; bindZ (q, i, z))
        | bindX (q, i, REAL r)    = (print "bindX: "; bindR (q, i, r))
        | bindX (q, i, STRING s)  = (print "bindX: "; bindS (q, i, s))
        | bindX (q, i, BLOB b)    = (print "bindX: "; bindB (q, i, b))
        | bindX (q, i, NULL)      = (print "bindX: "; bindN (q, i))
      
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
      val fetchS = fetchWrap ("fetchS", P.fetchS, fn x => x)
      fun fetchN (q, i) = print ("fetchN " ^ Int.toString i ^ ": NULL\n")
      val fetchX = fetchWrap ("fetchX", P.fetchX, 
                        fn (INTEGER z) => Int64.toString z
                         | (REAL r)    => Real.toString r
                         | (STRING s)  => s
                         | (BLOB b)    => Int.toString (Word8Vector.length b)
                         | NULL        => "(NULL)")
      
      val meta = wrap (P.meta, "meta")
   end

structure Prim = PrimDebug(Prim)
