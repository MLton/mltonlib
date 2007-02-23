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
structure Query =
   struct
      (* Cry ... *)
      type 'a oF = Prim.query -> 'a
      type ('b, 'c) oN = Prim.query * (unit -> 'b) -> 'c
      type 'd iF = Prim.query * 'd -> unit
      type ('e, 'f) iN = Prim.query * 'e -> 'f
      type ('i, 'o, 'w, 'x, 'y, 'z) acc = 
         string list * 'o oF * ('w, 'x) oN * int * 'i iF * ('y, 'z) iN * int
      
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
                    free:       Prim.query list ref, (* common to all queries in the DB *)
                    available:  Prim.query list ref, (* specific to this query *)
                    used:       int ref }
      
      type ('i, 'o) t = { pool:  pool Ring.t MLton.Finalizable.t,
                          iF:    Prim.query * 'i -> unit,
                          oF:    Prim.query -> 'o }
      
      (* Close unused queries, one at a time *)
      fun cleanup free =
         let
            (* Carefully tidy the list up in case of exception *)
            fun helper [] = ()
              | helper (x :: r) = (Prim.finalize x; free := r; helper r)
         in
            MLton.Thread.atomically (fn () => helper (!free))
         end
      
      fun accessPool (pool, f) =
         MLton.Finalizable.withValue (pool, fn x => f (Ring.get x))
      
      fun peek ({ pool, iF=_, oF=_ }, f) =
         accessPool (pool, fn { db, query, free, available, used } =>
         if !used = ~1 then raise Prim.Error "Database closed" else
         case !available of
            x :: _ => (cleanup free; f x)
          | [] => 
               let
                  val () = cleanup free
                  val pq = Prim.prepare (db, query)
                  val () = available := pq :: !available
               in
                  f pq
               end)
      
      fun alloc ({ pool, iF, oF}, i) =
         accessPool (pool, fn { db, query, free, available, used } =>
         if !used = ~1 then raise Prim.Error "Database closed" else
         let
            val () = cleanup free
            val pq = case !available of
                        [] => (cleanup free; Prim.prepare (db, query))
                      | x :: r => (available := r; x)
            val () = used := !used + 1
            val () = iF (pq, i)
         in
            (pq, oF)
         end)
      
      fun release ({ pool, iF=_, oF=_ }, pq) =
         accessPool (pool, fn {db=_, query=_, free=_, available, used } =>
         if !used = ~1 then raise Prim.Error "SQLite wrapper bug: cannot release closed query" else
         if !used = 0 then raise Prim.Error "SQLite wrapper bug: too many releases" else
         ( Prim.reset pq;
           Prim.clearbindings pq;
           used := !used - 1;
           available := pq :: !available))
      
      fun oF0 _ = ()
      fun oN0 (_, n) = n ()
      val oI0 = 0
      fun iF0 (_, ()) = ()
      fun iN0 (_, x) = x
      val iI0 = 1
      
      local
         fun close l =
            case Ring.get l of { db=_, query=_, free, available, used } =>
            if !used <> 0 then raise Prim.Error "SQLite wrapper bug: finalizing in-use query" else
            (* We don't need to lock the free-list or pool-ring:
             * Operations on them (adds/removes) are in a critical section;
             * this method is only run from a distinct GC thread.
             * Also, the available list is never accessed except by the 
             * methods above which operate on a query. This is a finalizer
             * for the query so there can be no further references.
             *)
            (Ring.remove l; free := !available @ !free)
            (* This is unneeded as the link is no longer referenced anywhere:
             *   (available := []; used := ~1)
             *)
      in
         fun prepare { ring, hooks=_, auth=_ } qt =
            case Ring.get ring of { db, query=_, free, available=_, used=_ } =>
            Fold.fold (([qt], oF0, oN0, oI0, iF0, iN0, iI0),
                       fn (ql, oF, _, oI, iF, _, iI) => 
                       let
                           val qs = concat (rev ql)
                           val () = cleanup free
                           val q = Prim.prepare (db, qs)
                       in 
                           if Prim.cols q < oI
                           then (Prim.finalize q;
                                 raise Prim.Error "Insufficient output columns\
                                                  \ to satisfy prototype")
                           else
                           if Prim.bindings q + 1 <> iI
                           then (Prim.finalize q;
                                 raise Prim.Error "Too many query parameters\
                                                  \ for specified prototype")
                           else
                           let
                              val i = { db = db, query = qs, free = free,
                                        available = ref [q], used = ref 0 }
                              val pool = MLton.Thread.atomically 
                                            (fn () => MLton.Finalizable.new 
                                                         (Ring.add (ring, i)))
                           in
                              MLton.Finalizable.addFinalizer (pool, close);
                              { pool = pool, iF = iF, oF = oF }
                           end
                       end)
         end
      
      (* terminate an expression with this: *)
      val $ = $
      
      (* the ignore is just to silence a warning. it really will be a unit *)
      fun iFx f (iN, iI) (q, a) = f (q, iI, iN (q, a))
      fun iNx f (iN, iI) (q, a & y) = (ignore (f (q, iI, iN (q, a))); y)
      fun iMap f = Fold.step1 (fn (qs, (ql, oF, oN, oI, _, iN, iI)) => 
                                  (qs :: "?" :: ql, oF, oN, oI, 
                                   iFx f (iN, iI), iNx f (iN, iI), iI + 1))
      fun iB z = iMap Prim.bindB z
      fun iR z = iMap Prim.bindR z
      fun iI z = iMap Prim.bindI z
      fun iZ z = iMap Prim.bindZ z
      fun iN z = iMap Prim.bindN z
      fun iS z = iMap Prim.bindS z
      fun iX z = iMap Prim.bindX z
      
      fun oFx f (oN, oI) q = oN (q, fn () => f (q, oI))
      fun oNx f (oN, oI) (q, n) = oN (q, fn () => f (q, oI)) & n ()
      fun oMap f = Fold.step0 (fn (ql, _, oN, oI, iF, iN, iI) => 
                                  (ql, oFx f (oN, oI), oNx f (oN, oI), oI+1, 
                                   iF, iN, iI))
      fun oB z = oMap Prim.fetchB z
      fun oR z = oMap Prim.fetchR z
      fun oI z = oMap Prim.fetchI z
      fun oZ z = oMap Prim.fetchZ z
      fun oN z = oMap Prim.fetchN z
      fun oS z = oMap Prim.fetchS z
      fun oX z = oMap Prim.fetchX z
      
      fun fetchA (q, m) = Vector.tabulate (Prim.cols q, fn i => m (q, i))
      fun oFAx f oN q = oN (q, fn () => fetchA (q, f))
      fun oNAx f oN (q, n) = oN (q, fn () => fetchA (q, f)) & n ()
      fun oMapA f = Fold.step0 (fn (ql, _, oN, oI, iF, iN, iI) => 
                                   (ql, oFAx f oN, oNAx f oN, oI, 
                                    iF, iN, iI))
      fun oAB z = oMapA Prim.fetchB z
      fun oAR z = oMapA Prim.fetchR z
      fun oAI z = oMapA Prim.fetchI z
      fun oAZ z = oMapA Prim.fetchZ z
      fun oAN z = oMapA Prim.fetchN z
      fun oAS z = oMapA Prim.fetchS z
      fun oAX z = oMapA Prim.fetchX z
   end      
