structure Function =
   struct
      type t = (Prim.context * Prim.value vector -> unit) * int
      
      type 'a oF = Prim.value vector -> 'a
      type ('b, 'c) oN = Prim.value vector * (unit -> 'b) -> 'c
      type ('a, 'b, 'c) acc = int * 'a oF * ('b, 'c) oN
      
      type ('v, 'a, 'b, 'c, 'd, 'e, 'f) input = 
          (('a, 'v, 'b) acc, ('b, 'c, ('b, 'c) pair) acc, 'd, 'e, 'f) Fold.step0
      type ('v, 'a, 'b, 'c, 'd, 'e) fnX = 
          ((unit, 'a, 'a) acc, ('b, 'c, 'd) acc, ('b -> 'v) -> t, 'e) Fold.t 
      
      val iI0 = 0
      fun iF0 _ = ()
      fun iN0 (_, n) = n ()
      
      fun fnMap r = Fold.fold ((iI0, iF0, iN0),
                               fn (iI, iF, _) => fn f => 
                               (fn (c, v) => r (c, f (iF v)), iI))
      fun fnB z = fnMap Prim.resultB z
      fun fnR z = fnMap Prim.resultR z
      fun fnI z = fnMap Prim.resultI z
      fun fnZ z = fnMap Prim.resultZ z
      fun fnS z = fnMap Prim.resultS z
      fun fnX z = fnMap Prim.resultX z
      
      (* terminate an expression with this: *)
      val $ = $
      
      fun iFx f (iN, iI) v = iN (v, fn () => f (Vector.sub (v, iI)))
      fun iNx f (iN, iI) (v, n) = iN (v, fn () => f (Vector.sub (v, iI))) & n ()
      fun iMap f = Fold.step0 (fn (iI, iF, iN) => 
                                  (iI+1, iFx f (iN, iI), iNx f (iN, iI)))
      fun iB z = iMap Prim.valueB z
      fun iR z = iMap Prim.valueR z
      fun iI z = iMap Prim.valueI z
      fun iZ z = iMap Prim.valueZ z
      fun iS z = iMap Prim.valueS z
      fun iX z = iMap Prim.valueX z
   end
