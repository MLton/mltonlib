structure Template =
   struct
      fun outputEnds (_, _, r) = r
      fun inputEnds ((oF, db, q), _, b) =
         let
            val q = Prim.prepare (db, q)
            val () = b q
            
            fun exec f = oF (q, 0, f)
         in
            (q, exec)
         end
      
      type ('o, 'r) oF = Prim.query * int * 'o -> 'r
      type ('of, 'i, 'r) iF = (('of, 'r) oF * Prim.db * string) * int * (Prim.query -> unit) -> 'i
      type ('o, 'of, 'i, 'r) acc = string list * ('o, 'r) oF * ('of, 'i, 'r) iF
      
      fun query q =
         Foldr.fold (([], outputEnds, inputEnds), 
                     fn (ql, oF, iF) => fn db =>
                     iF ((oF, db, concat (q::ql)), 1, fn _ => ()))
      
      (* terminate an execution with this: *)
      val $ = $
      
      (* typecast a single column and set it up as an argument *)
      fun oFetch m s (q, i, f) = s (q, i+1, f (m (q, i)))
(*    fun oMap f = Foldr.step1 (fn (q, (ql, oF, iF)) => (q :: ql, oFetch f oF, iF)) *)
      fun oMap f = Foldr.step0 (fn (ql, oF, iF) => (ql, oFetch f oF, iF))
      fun oB z = oMap Prim.fetchB z
      fun oR z = oMap Prim.fetchR z
      fun oI z = oMap Prim.fetchI z
      fun oZ z = oMap Prim.fetchZ z
      fun oS z = oMap Prim.fetchS z
      fun oX z = oMap Prim.fetchX z
      
      (* typecast all columns to a vector and set it up as an argument *)
      fun fetchA (q, m) = Vector.tabulate (Prim.cols q, fn i => m (q, i))
      fun oFetchA m s (q, i, f) = s (q, i, f (fetchA (q, m)))
      fun oMapA f = Foldr.step0 (fn (ql, oF, iF) => (ql, oFetchA f oF, iF))
      fun oAB z = oMapA Prim.fetchB z
      fun oAR z = oMapA Prim.fetchR z
      fun oAI z = oMapA Prim.fetchI z
      fun oAZ z = oMapA Prim.fetchZ z
      fun oAS z = oMapA Prim.fetchS z
      fun oAX z = oMapA Prim.fetchX z
      
      fun iBind m s (z, i, b) x = s (z, i+1, fn q => (b q; m (q, i, x)))
      fun iMap f = Foldr.step1 (fn (q, (ql, oF, iF)) => ("?" :: q :: ql, oF, iBind f iF))
      fun iB z = iMap Prim.bindB z
      fun iR z = iMap Prim.bindR z
      fun iI z = iMap Prim.bindI z
      fun iZ z = iMap Prim.bindZ z
      fun iS z = iMap Prim.bindS z
      fun iX z = iMap Prim.bindX z
   end      
