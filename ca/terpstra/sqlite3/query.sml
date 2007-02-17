structure Query =
   struct
      (* Cry ... *)
      type 'a oF = Prim.query -> 'a
      type ('b, 'c) oN = Prim.query * (unit -> 'b) -> 'c
      type 'd iF = Prim.query * 'd -> unit
      type ('e, 'f) iN = Prim.query * 'e -> int * 'f
      type ('i, 'o, 'w, 'x, 'y, 'z) acc = string list * 'o oF * ('w, 'x) oN * int * 'i iF * ('y, 'z) iN
      type ('v, 'i, 'o, 'p, 'q, 'a, 'b, 'x, 'y, 'z) output = 
           (('i, 'o, 'v, 'p,            'a, 'b) acc, 
            ('i, 'p, 'q, ('p, 'q) pair, 'a, 'b) acc, 
            'x, 'y, 'z) Fold.step0
      type ('v, 'i, 'o, 'j, 'k, 'a, 'b, 'x, 'y, 'z) input = 
           (string, ('i, 'o, 'a, 'b, 'j, 'v) acc, 
                    ('j, 'o, 'a, 'b, ('j, 'k) pair, 'k) acc, 
                    'x, 'y, 'z) Fold.step1
      
      (* We need to be able to create new queries for recursive usage.
       * Each prepared statement has only a single VM, so we need a factory
       * to support reentrant processing. The used counter records how many
       * outstanding queries there are (~1 means DB closed). The pool saves
       * previously allocated prepared statements for quick re-use.
       *)
      
      type pool = { db:         Prim.db,
                    query:      string,
                    available:  Prim.query list ref,
                    used:       int ref }
      
      type ('i, 'o) t = { pool:  pool Ring.t MLton.Finalizable.t,
                          iF:    Prim.query * 'i -> unit,
                          oF:    Prim.query -> 'o }
      
      fun accessPool (pool, f) =
         MLton.Finalizable.withValue (pool, fn x => f (Ring.get x))
      
      fun peek ({ pool, iF=_, oF=_ }, f) =
         accessPool (pool, fn { db, query, available, used } =>
         if !used = ~1 then raise Prim.Error "Database closed" else
         case !available of
            x :: r => f x
          | [] => 
               let
                  val pq = Prim.prepare (db, query)
                  val () = available := pq :: !available
               in
                  f pq
               end)
      
      fun alloc ({ pool, iF, oF}, i) =
         accessPool (pool, fn { db, query, available, used } =>
         if !used = ~1 then raise Prim.Error "Database closed" else
         let
            val pq = case !available of
                        [] => Prim.prepare (db, query)
                      | x :: r => (available := r; x)
            val () = used := !used + 1
            val () = iF (pq, i)
         in
            (pq, oF)
         end)
      
      fun release ({ pool, iF=_, oF=_ }, pq) =
         accessPool (pool, fn {db=_, query=_, available, used } =>
         if !used = ~1 then raise Prim.Error "SQLite wrapper bug: cannot release closed query" else
         if !used = 0 then raise Prim.Error "SQLite wrapper bug: too many releases" else
         ( Prim.reset pq;
           Prim.clearbindings pq;
           used := !used - 1;
           available := pq :: !available))
      
      fun oF0 _ = ()
      fun oN0 (q, n) = n ()
      val oI0 = 0
      fun iF0 (q, ()) = ()
      fun iN0 (q, x) = (1, x)
      
      local
         fun forceClose q = Prim.finalize q handle _ => ()
         fun close l =
            case Ring.get l of { db=_, query=_, available, used } =>
            if !used <> 0 then raise Prim.Error "SQLite wrapper bug: finalizing in-use query" else
            ( List.app forceClose (!available);
              available := [];
              used := ~1;
              Ring.remove l
              )
      in
         fun prepare { ring, hooks } qt =
            case Ring.get ring of { db, query=_, available=_, used=_ } =>
            Fold.fold (([qt], oF0, oN0, oI0, iF0, iN0),
                       fn (ql, oF, _, oI, iF, _) => 
                       let
                           val qs = concat (rev ql)
                           val q = Prim.prepare (db, qs)
                       in 
                           if Prim.cols q < oI
                           then (Prim.finalize q;
                                 raise Prim.Error "insufficient output columns\
                                                  \ to satisfy prototype")
                           else
                           let
                              val pool = MLton.Finalizable.new (
                                            Ring.add ({ db = db, 
                                                        query = qs, 
                                                        available = ref [q], 
                                                        used = ref 0 }, ring))
                              val out = { pool = pool, iF = iF, oF = oF }
                           in
                              MLton.Finalizable.addFinalizer (pool, close);
                              out
                           end
                       end)
         end
      
      (* terminate an expression with this: *)
      val $ = $
      
      fun iFx f iN (q, a) = case iN (q, a) of (i, x) => f (q, i, x)
      fun iNx f iN (q, a & y) = case iN (q, a) of (i, x) => (f (q, i, x); (i+1, y))
      fun iMap f = Fold.step1 (fn (qs, (ql, oF, oN, oI, iF, iN)) => 
                                  (qs :: "?" :: ql, oF, oN, oI, iFx f iN, iNx f iN))
      fun iB z = iMap Prim.bindB z
      fun iR z = iMap Prim.bindR z
      fun iI z = iMap Prim.bindI z
      fun iZ z = iMap Prim.bindZ z
      fun iS z = iMap Prim.bindS z
      fun iX z = iMap Prim.bindX z
      
      fun oFx f (oN, oI) q = oN (q, fn () => f (q, oI))
      fun oNx f (oN, oI) (q, n) = oN (q, fn () => f (q, oI)) & n ()
      fun oMap f = Fold.step0 (fn (ql, oF, oN, oI, iF, iN) => 
                                  (ql, oFx f (oN, oI), oNx f (oN, oI), oI+1, iF, iN))
      fun oB z = oMap Prim.fetchB z
      fun oR z = oMap Prim.fetchR z
      fun oI z = oMap Prim.fetchI z
      fun oZ z = oMap Prim.fetchZ z
      fun oS z = oMap Prim.fetchS z
      fun oX z = oMap Prim.fetchX z
      
      fun fetchA (q, m) = Vector.tabulate (Prim.cols q, fn i => m (q, i))
      fun oFAx f oN q = oN (q, fn () => fetchA (q, f))
      fun oNAx f oN (q, n) = oN (q, fn () => fetchA (q, f)) & n ()
      fun oMapA f = Fold.step0 (fn (ql, oF, oN, oI, iF, iN) => 
                                   (ql, oFAx f oN, oNAx f oN, oI, iF, iN))
      fun oAB z = oMapA Prim.fetchB z
      fun oAR z = oMapA Prim.fetchR z
      fun oAI z = oMapA Prim.fetchI z
      fun oAZ z = oMapA Prim.fetchZ z
      fun oAS z = oMapA Prim.fetchS z
      fun oAX z = oMapA Prim.fetchX z
   end      
