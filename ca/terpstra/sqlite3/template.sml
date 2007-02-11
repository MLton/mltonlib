structure Template =
   struct
      type 'a oF = Prim.query -> 'a
      type ('b, 'c) oN = Prim.query * (unit -> 'b) -> 'c
      type 'd iF = 'd * string -> Prim.query * int
      type ('i, 'o, 'x, 'y) acc = string list * 'o oF * ('x, 'y) oN * int * 'i iF
      type ('v, 'i, 'o, 'x, 'y, 'a, 'b, 'c) output = (('i, 'o, 'v, 'x) acc, ('i, 'x, 'y, ('x, 'y) pair) acc, 'a, 'b, 'c) Fold.step0
      type ('v, 'i, 'o, 'x, 'y, 'a, 'b, 'c) input = (string, ('i, 'o, 'x, 'y) acc, (('i, 'v) pair, 'o, 'x, 'y) acc, 'a, 'b, 'c) Fold.step1
      
      fun oF0 _ = ()
      fun oN0 (q, n) = n ()
      val oI0 = 0
      fun iF0 (db, qs) = (Prim.prepare (db, qs), 1)
      
      fun query qs = Fold.fold (([qs], oF0, oN0, oI0, iF0),
                                fn (ql, oF, _, _, iF) => 
                                let val qs = concat (rev ql)
                                in fn arg => 
                                   case iF (arg, qs) of (q, _) => (q, oF)
                                end)
      
      fun iFx f iF (a & x, qs) = case iF (a, qs) of (q, i) => (f (q, i, x); (q, i+1))
      fun iMap f = Fold.step1 (fn (qs, (ql, oF, oN, oI, iF)) => 
                                  (qs :: "?" :: ql, oF, oN, oI, iFx f iF))
      fun iB z = iMap Prim.bindB z
      fun iR z = iMap Prim.bindR z
      fun iI z = iMap Prim.bindI z
      fun iZ z = iMap Prim.bindZ z
      fun iS z = iMap Prim.bindS z
      fun iX z = iMap Prim.bindX z
      
      fun oFx f (oN, oI) q = oN (q, fn () => f (q, oI))
      fun oNx f (oN, oI) (q, n) = oN (q, fn () => f (q, oI)) & n ()
      fun oMap f = Fold.step0 (fn (ql, oF, oN, oI, iF) => 
                                  (ql, oFx f (oN, oI), oNx f (oN, oI), oI+1, iF))
      fun oB z = oMap Prim.fetchB z
      fun oR z = oMap Prim.fetchR z
      fun oI z = oMap Prim.fetchI z
      fun oZ z = oMap Prim.fetchZ z
      fun oS z = oMap Prim.fetchS z
      fun oX z = oMap Prim.fetchX z
      
      fun fetchA (q, m) = Vector.tabulate (Prim.cols q, fn i => m (q, i))
      fun oFAx f oN q = oN (q, fn () => fetchA (q, f))
      fun oNAx f oN (q, n) = oN (q, fn () => fetchA (q, f)) & n ()
      fun oMapA f = Fold.step0 (fn (ql, oF, oN, oI, iF) => 
                                   (ql, oFAx f oN, oNAx f oN, oI, iF))
      fun oAB z = oMapA Prim.fetchB z
      fun oAR z = oMapA Prim.fetchR z
      fun oAI z = oMapA Prim.fetchI z
      fun oAZ z = oMapA Prim.fetchZ z
      fun oAS z = oMapA Prim.fetchS z
      fun oAX z = oMapA Prim.fetchX z
      
      (* terminate an execution with this: *)
      val $ = $
   end      
