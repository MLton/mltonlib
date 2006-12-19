signature BTREE =
  sig
    type key
    type 'val t
    
    val empty: 'val t
    
    val app: ('val -> unit) -> 'val t -> unit
    val appk: ((key * 'val) -> unit) -> 'val t -> unit
    val map: ('val -> 'new) -> 'val t -> 'new t
    val mapk: ((key * 'val) -> 'new) -> 'val t -> 'new t
    
    val fold: (key * 'val * 'a -> 'a) -> 'a -> 'val t -> 'a
    val foldr: (key * 'val * 'a -> 'a) -> 'a -> 'val t -> 'a
    
    val get: 'val t -> key -> 'val option
    val insert: 'val t -> (key * 'val) -> 'val t
    
    datatype 'val iterator = 
      Iter of key * 'val * (unit -> 'val iterator) option
    val front: 'val t -> (unit -> 'val iterator) option
  end
    
functor BTree(Order : ORDER) : BTREE =
  struct
    open Order
    
    type key = Order.t
    datatype colour = Red | Black 
    datatype 'val t = Node of colour * 'val t * (key * 'val) * 'val t | Leaf
    
    val empty = Leaf 
    
    fun app f Leaf = ()
      | app f (Node (c, l, (y, v), r)) = 
          (app f l; f v; app f r)
    
    fun appk f Leaf = ()
      | appk f (Node (c, l, (y, v), r)) = 
          (appk f l; f (y, v); appk f r)
    
    fun map f Leaf = Leaf
      | map f (Node (c, l, (y, v), r)) = 
          Node (c, map f l, (y, f v), map f r)
    
    fun mapk f Leaf = Leaf
      | mapk f (Node (c, l, (y, v), r)) = 
          Node (c, mapk f l, (y, f (y, v)), mapk f r)
    
    fun fold f a Leaf = a
      | fold f a (Node (c, l, (y, v), r)) =
          fold f (f (y, v, fold f a l)) r
    
    fun foldr f a Leaf = a
      | foldr f a (Node (c, l, (y, v), r)) =
          foldr f (f (y, v, foldr f a r)) l
    
    fun get Leaf x = NONE
      | get (Node (_, l, (y, v), r)) x =
          if      x < y then get l x
          else if y < x then get r x
          else SOME v
    
    fun balance x = case x of
	  (Black, Node (Red, Node (Red, a, x, b), y, c), z, d) =>
		Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
	| (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d) =>
		Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
	| (Black, a, x, Node (Red, Node (Red, b, y, c), z, d)) =>
		Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
	| (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) =>
		Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
	| (a, b, c, d) =>
		Node (a, b, c, d)
      
    fun insert t (x, v) =
      let
        fun ins Leaf = Node (Red, Leaf, (x, v), Leaf)
          | ins (Node (c, a, (y, v'), b)) =
              if      x < y then balance (c, ins a, (y, v'), b)
              else if y < x then balance (c, a, (y, v'), ins b)
              else balance (c, a, (x, v), b)
      in
        case ins t of
            Node (_, a, y, b) => Node (Black, a, y, b)
	  | Leaf => Leaf
      end
    
    datatype 'val iterator = 
      Iter of key * 'val * (unit -> 'val iterator) option
      
    fun front t =
      let
        datatype 'val stack = Parent of key * 'val * 'val t
        fun goleft (Leaf, []) = NONE
          | goleft (Leaf, stack) = SOME (spit stack)
          | goleft (Node (_, l, (k, v), r), stack) = 
              goleft (l, Parent (k, v, r) :: stack)
        and spit [] () = raise Overflow (* unreachable *)
          | spit (Parent (k, v, r) :: stack) () = 
              Iter (k, v, goleft (r, stack))
      in
        goleft (t, [])
      end
  end
