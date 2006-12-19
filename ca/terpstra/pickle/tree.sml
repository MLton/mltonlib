signature MAP =
  sig
    type k
    type 'a t
    
    val empty: 'a t
    
    val app:    (k * 'a -> unit) -> 'a t -> unit
    val filter: (k * 'a -> bool) -> 'a t -> 'a t
    val map:    (k * 'a -> k * 'b) -> 'a t -> 'b t
    
    val foldl: ((k * 'a) * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldr: ((k * 'a) * 'b -> 'b) -> 'b -> 'a t -> 'b
    
    val fromList:   (k * 'a) list   -> 'a t
    val fromVector: (k * 'a) vector -> 'a t
    
    val fetch:  'a t -> k -> 'a option
    val insert: 'a t -> k * 'a -> 'a t
    
    (* put smaller set on the right *)
    val union:        'a t * 'a t -> 'a t
    val intersection: 'a t * 'a t -> 'a t
    val difference:   'a t * 'a t -> 'a t
  end
    
signature SET =
  sig
    type k
    type t
    
    val empty: t
    
    val app:    (k -> unit) -> t -> unit
    val filter: (k -> bool) -> t -> t
    val map:    (k -> k   ) -> t -> t
    
    val foldl: (k * 'a -> 'a) -> 'a -> t -> 'a
    val foldr: (k * 'a -> 'a) -> 'a -> t -> 'a
    
    val fromList:   k list   -> t
    val fromVector: k vector -> t
    
    val member: t -> k -> bool
    val insert: t -> k -> t
    
    (* put smaller set on the right *)
    val union:        t * t -> t
    val intersection: t * t -> t
    val difference:   t * t -> t
  end
    
signature TREE_ORDER =
  sig
    type 'a member
    val order: 'a member * 'a member -> order
  end

functor Tree(O : TREE_ORDER) =
  struct
    open O
    datatype colour = Red | Black
    datatype 'a tree = Node of colour * 'a tree * 'a member * 'a tree | Leaf
    
    val empty = Leaf
    
    fun app f Leaf = ()
      | app f (Node (_, l, v, r)) = 
          (app f l; f v; app f r)
          
    fun map f Leaf = Leaf
      | map f (Node (c, l, v, r)) = 
          Node (c, map f l, f v, map f r)
    
    fun foldl f b Leaf = b
      | foldl f b (Node (c, l, v, r)) =
          foldl f (f (v, foldl f b l)) r
    
    fun foldr f b Leaf = b
      | foldr f b (Node (c, l, v, r)) =
          foldr f (f (v, foldr f b r)) l
    
    fun member Leaf _ = false
      | member (Node (_, l, v, r)) x =
          case order (x, v) of
              LESS    => member l x
            | GREATER => member r x
            | EQUAL   => true
    
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
      
    fun insert t x =
      let
        fun ins Leaf = Node (Red, Leaf, x, Leaf)
          | ins (Node (c, a, y, b)) =
              case order (x, y) of
                  LESS    => balance (c, ins a, y, b)
                | GREATER => balance (c, a, y, ins b)
                | EQUAL   => Node (c, a, x, b)
      in
        case ins t of
            Node (_, a, y, b) => Node (Black, a, y, b)
	  | Leaf => Leaf
      end
    
    fun fromList l =
      List.foldl (fn (v, t) => insert t v) empty l
    fun fromVector v =
      Vector.foldl (fn (v, t) => insert t v) empty v
    
    fun filter f t =
      foldl (fn (v, t) => if f v then insert t v else t) empty t
    
    fun union (x, y) =
      foldl (fn (v, t) => insert t v) x y
    fun intersection (x, y) =
      filter (member x) y
    fun difference (x, y) =
      filter (not o member y) x
  end

signature KEY_ORDER =
  sig
    type k
    val order: k * k -> order
  end

functor Set(O : KEY_ORDER) :> SET where type k = O.k =
  struct
    structure TO =
      struct
        type 'a member = O.k
        val order = O.order
      end
    
    structure Tree = Tree(TO)
    open Tree
    
    type k = O.k
    type t = unit tree
  end

functor Map(O : KEY_ORDER) :> MAP where type k = O.k =
  struct
    structure TO =
      struct
        type 'a member = O.k * 'a
        fun order ((x, _), (y, _)) = O.order (x, y)
      end
    
    structure Tree = Tree(TO)
    open Tree
    
    type k = O.k
    type 'a t = 'a tree
    
    fun fetch Leaf _ = NONE
      | fetch (Node (_, l, (k, v), r)) x =
          case O.order (x, k) of
              LESS    => fetch l x
            | GREATER => fetch r x
            | EQUAL   => SOME v
  end
