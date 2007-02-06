structure SQL =
   struct
      fun iA (oF, b, (db, q), _) () =
         let
            val q = Prim.prepare (db, q)
            val () = b q
            
            fun exec NONE = (Prim.finalize q; NONE)
              | exec (SOME f) =
                   if Prim.step q 
                   then SOME (oF (q, f, 0)) 
                   else (Prim.finalize q; NONE)
         in
            exec
         end
      
      fun oA (_, r, _) = r
      
      fun execute db q =
         Foldr.foldr (([], oA, iA), 
                      fn (ql, oF, iF) => iF (oF, fn _ => (), (db, concat (q::ql)), 1))
      
      fun oFetch m s (q, f, i) = s (q, f (m (q, i)), i+1) 
      fun oMap f = Foldr.step1 (fn (q, (ql, oF, iF)) => (q :: ql, f oF, iF))
      fun oB z = oMap (oFetch Prim.fetchB) z
      fun oR z = oMap (oFetch Prim.fetchR) z
      fun oI z = oMap (oFetch Prim.fetchI) z
      fun oZ z = oMap (oFetch Prim.fetchZ) z
      fun oS z = oMap (oFetch Prim.fetchS) z
      fun oX z = oMap (oFetch Prim.fetchX) z
      
      fun iBind m s (oF, b, d, i) x = s (oF, fn q => (b q; m (q, i, x)), d, i+1)
      fun iMap f = Foldr.step1 (fn (q, (ql, oF, iF)) => ("?" :: q :: ql, oF, f iF))
      fun iB z = iMap (iBind Prim.bindB) z
      fun iR z = iMap (iBind Prim.bindR) z
      fun iI z = iMap (iBind Prim.bindI) z
      fun iZ z = iMap (iBind Prim.bindZ) z
      fun iS z = iMap (iBind Prim.bindS) z
      fun iX z = iMap (iBind Prim.bindX) z
   end

(*
open SQL
val db = Prim.openDB "test.db"
val Q : real -> string -> int -> unit -> (string -> string -> bool) option -> bool option =
   execute db "select (a"oS", b"oS") from table where x="iR" and y="iS" and z="iI";" $
*)
