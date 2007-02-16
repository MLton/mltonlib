structure Function =
   struct
      type scalar = (Prim.context * Prim.value vector -> unit) * int
      type aggregate = Prim.aggregate * int
      
      type ('a, 'b, 'c) folder = {
         init: unit -> 'a,
         step: 'a * 'b -> 'a,
         finish: 'a -> 'c
      }
      
      type 'a iF = Prim.value vector -> 'a
      type ('b, 'c) iN = Prim.value vector * (unit -> 'b) -> 'c
      type ('a, 'b, 'c) acc = int * 'a iF * ('b, 'c) iN
      
      type ('v, 'a, 'b, 'c, 'd, 'e) fnX = 
          ((unit, 'a, 'a) acc, ('b, 'c, 'd) acc, ('b -> 'v) -> scalar, 'e) Fold.t 
      type ('v, 'a, 'b, 'c, 'd, 'e, 'f) aggrX = 
          ((unit, 'a, 'a) acc, ('b, 'c, 'd) acc, ('f, 'b, 'v) folder -> aggregate, 'e) Fold.t
      
      type ('v, 'a, 'b, 'c, 'd, 'e, 'f) input = 
          (('a, 'v, 'b) acc, ('b, 'c, ('b, 'c) pair) acc, 'd, 'e, 'f) Fold.step0
      type ('v, 'a, 'b, 'c) inputA = 
          ((unit, unit, unit) acc, ('v vector, unit, unit) acc, 'a, 'b, 'c) Fold.step0
      
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
      fun fnN z = fnMap Prim.resultN z
      
      fun aggrMap r = Fold.fold ((iI0, iF0, iN0),
                               fn (iI, iF, _) => fn { init, step, finish } =>
                               let
                                  fun finish1 c = r (c, finish (init ()))
                                  fun step1 x =
                                     let
                                        val acc = ref (init ())
                                        fun stepX (_, v) = 
                                           (acc := step (!acc, iF v); 
                                            Prim.AGGREGATE (stepX, finishX))
                                        and finishX c = r (c, finish (!acc))
                                      in
                                        stepX x
                                      end
                               in
                                  (Prim.AGGREGATE (step1, finish1), iI)
                               end)
      fun aggrB z = aggrMap Prim.resultB z
      fun aggrR z = aggrMap Prim.resultR z
      fun aggrI z = aggrMap Prim.resultI z
      fun aggrZ z = aggrMap Prim.resultZ z
      fun aggrS z = aggrMap Prim.resultS z
      fun aggrX z = aggrMap Prim.resultX z
      fun aggrN z = aggrMap Prim.resultN z
      
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
      
      fun iAFx f v = Vector.map f v
      fun iANx iF (v, n) = case iF v of () => () (* plug the type *)
      fun iAMap f = Fold.step0 (fn (_, iF, _) => (~1, iAFx f, iANx iF))
      fun iAB z = iAMap Prim.valueB z
      fun iAR z = iAMap Prim.valueR z
      fun iAI z = iAMap Prim.valueI z
      fun iAZ z = iAMap Prim.valueZ z
      fun iAS z = iAMap Prim.valueS z
      fun iAX z = iAMap Prim.valueX z
   end
