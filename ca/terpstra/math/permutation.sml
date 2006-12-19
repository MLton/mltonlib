signature FINITE_PERMUTATION =
  sig
    include PERMUTATION
    
    (* a bijection exists between naturals and permutations *)
    val toInt:  t -> int
    val fromInt: int -> t
    val toLargeInt: t -> LargeInt.int
    val fromLargeInt: LargeInt.int -> t
    
    (* an alternate representation exists as cycles *)
    val fromCycle: int list -> t
    val fromCycles: int list list -> t
    val toCycles: t -> int list list
  end

structure FinitePermutation : FINITE_PERMUTATION =
  struct
    type v = int
    type t = int Vector.vector
    val order = COUNTABLE
    
    val associative = ()
    val one = Vector.tabulate (0, fn _ => 0)
    
    fun EVAL f x =
      if x >= Vector.length f then x else
      Vector.sub (f, x)
    
    fun EQ (x, y) = 
      let
        val lx = Vector.length x
        val ly = Vector.length y
      in
        if lx < ly 
        then Vector.foldli (fn (i, v, a) => a andalso v = EVAL x i) true y
        else Vector.foldli (fn (i, v, a) => a andalso v = EVAL y i) true x
      end
    
    fun MUL (x, y) =
      let
        val lx = Vector.length x
        val ly = Vector.length y
        val l = if lx < ly then ly else lx
      in
        Vector.tabulate (l, fn i => EVAL x (EVAL y i))
      end
    
    fun INV x = 
      let
        val y = Array.array (Vector.length x, 0)
      in
        Vector.appi (fn (i, v) => Array.update (y, v, i)) x;
        Array.vector y
      end
    
    fun toInt p =
      let
        fun helper p =
          let
            val l = Vector.length p
          in
            if l < 2 then (1, 0) else
            let
              val i = l - 1
              val s = VectorSlice.slice (p, 0, SOME i)
              val x = Vector.sub (p, i)
              val v = VectorSlice.map (fn y => if y > x then y-1 else y) s
              val (fact, out) = helper v
            in
              (fact * l, out + (i - x) * fact)
            end
          end
      in
        #2 (helper p)
      end
    
    fun toLargeInt p =
      let
        fun helper p =
          let
            val l = Vector.length p
          in
            if l < 2 then (1, 0) else
            let
              val i = l - 1
              val s = VectorSlice.slice (p, 0, SOME i)
              val x = Vector.sub (p, i)
              val v = VectorSlice.map (fn y => if y > x then y-1 else y) s
              val (fact, out) = helper v
              val y = i - x
              open LargeInt
            in
              (fact * fromInt l, out + fromInt y * fact)
            end
          end
      in
        #2 (helper p)
      end
    
    fun fromInt x =
      if x = 0 then Vector.tabulate (0, fn _ => 0) else
      let
        fun grow (l, f) =
          if f > x then (l, f) else
          grow (l+1, f*(l+1))
        val (l, f) = grow (0, 1)
        fun helper (1, _, _) = Vector.tabulate (1, fn _ => 0)
          | helper (l, f, x) =
          let
            val (l1, f) = (l - 1, f div l)
            val (q, r) = (x div f, x mod f)
            val p = helper (l1, f, r)
            val z = (l1 - q)
            fun build i = 
              if i = l1 then z else
              let val y = Vector.sub (p, i) in
              if y >= z then y+1 else y end
          in
            Vector.tabulate (l, build)
          end
      in
        helper (l, f, x)
      end
    
    fun fromLargeInt x =
      if x = 0 then Vector.tabulate (0, fn _ => 0) else
      let
        fun grow (l, f) =
          if f > x then (l, f) else
          grow (l + 1, LargeInt.* (f, LargeInt.fromInt (l + 1)))
        val (l, f) = grow (0, 1)
        fun helper (1, _, _) = Vector.tabulate (1, fn _ => 0)
          | helper (l, f, x) =
          let
            val (l1, f) = (l - 1, LargeInt.div (f, LargeInt.fromInt l))
            val (q, r) = IntInf.quotRem (x, f)
            val p = helper (l1, f, r)
            val z = (l1 - LargeInt.toInt q)
            fun build i = 
              if i = l1 then z else
              let val y = Vector.sub (p, i) in
              if y >= z then y+1 else y end
          in
            Vector.tabulate (l, build)
          end
      in
        helper (l, f, x)
      end
    
    fun fromCycle c =
      let
        val m = List.foldl (fn (x, a) => if x < a then a else x) 0 c
        val a = Array.tabulate (m+1, fn i => i)
        fun helper [] = ()
          | helper (x :: []) = Array.update (a, x, List.hd c)
          | helper (x :: y :: r) = (
          Array.update (a, x, y);
          helper (y :: r))
      in
        helper c;
        Array.vector a
      end
    
    fun fromCycles l =
      List.foldl (fn (x, a) => MUL (fromCycle x, a)) one l
    
    fun toCycles p =
      let
        val a = Array.tabulate (Vector.length p, fn i => false)
        fun trace i =
          if Array.sub (a, i) then [] else
          (Array.update (a, i, true);
           i :: trace (Vector.sub (p, i)))
        fun scan (i, l) =
          if i = l then [] else
          case trace i of 
              [] => scan (i+1, l)
            | _ :: [] => scan (i+1, l)
            | x => x :: scan (i+1, l)
      in
        scan (0, Vector.length p)
      end
  end

(*
fun test (x, e) =
  if x = e then () else
  let
    val p = FinitePermutation.fromLargeInt (LargeInt.fromInt x)
    val y = FinitePermutation.toLargeInt p
  in
    print (LargeInt.toString y ^ ":");
    Vector.app (fn y => print (" " ^ Int.toString y)) p;
    print "\n";
    test (x+1, e)
  end

val () = test (0, 720)

val p = FinitePermutation.fromCycles [[5, 4], [3], [], [2, 6, 3]]
val () = Vector.app (fn y => print (" " ^ Int.toString y)) p
val () = print "\n"

val p = FinitePermutation.fromCycles [[5, 4], [3], [], [2, 6, 3]]
val cs = FinitePermutation.toCycles p
val pc = List.app (fn y => print (" " ^ Int.toString y))
val pcs = List.app (fn y => (pc y; print "\n"))
val () = pcs cs
*)
