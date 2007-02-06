structure SQL =
   struct
      type db = Prim.db
      
      exception Retry = Prim.Retry
      exception Abort = Prim.Abort
      exception Fail  = Prim.Fail
      
      val openDB  = Prim.openDB
      val closeDB = Prim.closeDB
      
      fun outputEnds (_, _, f) = f ()
      fun inputEnds ((oF, db, q), _, b) () =
         let
            val q = Prim.prepare (db, q)
            val () = b q
            
            fun cancel () = 
              Prim.finalize q
            
            fun step f =
               if Prim.step q 
               then SOME (oF (q, 0, f)) 
               else (Prim.finalize q; NONE)
            
            fun app f =
               if Prim.step q
               then (oF (q, 0, f); app f)
               else Prim.finalize q
            
            fun map l f =
               if Prim.step q
               then map (oF (q, 0, f) :: l) f
               else (Prim.finalize q; Vector.fromList (List.rev l))
            
            fun meta () = {
               names = Prim.names q,
               databases = Prim.databases q,
               decltypes = Prim.decltypes q,
               tables = Prim.tables q,
               origins = Prim.origins q }
         in
            { step = step, app = app, map = map [], cancel = cancel, meta = meta }
         end
      
      fun execute db q =
         Foldr.foldr (([], outputEnds, inputEnds), 
                      fn (ql, oF, iF) => iF ((oF, db, concat (q::ql)), 1, fn _ => ()))
      
      (* typecast a single column and set it up as an argument *)
      fun oFetch m s (q, i, f) = s (q, i+1, f (m (q, i)))
      fun oMap f = Foldr.step1 (fn (q, (ql, oF, iF)) => (q :: ql, oFetch f oF, iF))
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
      
      fun i0 f () = f ()
      fun i1 f (a) = f a ()
      fun i2 f (a, b) = f a b ()
      fun i3 f (a, b, c) = f a b c ()
      fun i4 f (a, b, c, d) = f a b c d ()
      fun i5 f (a, b, c, d, e) = f a b c d e ()
      
      fun o0 f = f (fn () => ())
      fun o1 f = f (fn a => fn () => (a))
      fun o2 f = f (fn a => fn b => fn () => (a, b))
      fun o3 f = f (fn a => fn b => fn c => fn () => (a, b, c))
      fun o4 f = f (fn a => fn b => fn c => fn d => fn () => (a, b, c, d))
      fun o5 f = f (fn a => fn b => fn c => fn d => fn e => fn () => (a, b, c, d, e))
   end
(*
open SQL
val db = Prim.openDB "test.db"
val Q : real * string * int -> unit -> (string * string) option =
   o2 (i3 (execute db "select (a"oS", b"oS") from table where x="iR" and y="iS" and z="iI";" $))
*)
