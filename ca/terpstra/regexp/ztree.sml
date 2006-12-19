signature ORDER =
  sig
    type t
    val < : t * t -> bool
  end

signature ZTREE =
  sig
    type key
    datatype 'val t = 
      Leaf of 'val | 
      Node of 'val t * key * 'val t
    
    val uniform: 'val -> 'val t
    val range: ('val * key * key * 'val) -> 'val t
    val size: 'val t -> int
    
    (* compare two ZTrees for structural equality (balance must match) *)
    val equal: ('val * 'val -> bool) -> ('val t * 'val t) -> bool
    
    val app: ('val -> unit) -> 'val t -> unit
    val map: ('val -> 'new) -> 'val t -> 'new t
    val fold: (key option * 'val * key option * 'a -> 'a) -> 'a -> 'val t -> 'a
    val foldr: (key option * 'val * key option * 'a -> 'a) -> 'a -> 'val t -> 'a
    val lookup: 'val t -> key -> 'val
    
    datatype 'val iterator = 
      Iter of 'val * key option * (unit -> 'val iterator)
    val front: 'val t -> 'val iterator
    val back: 'val t -> 'val iterator
    val fromFront: 'val iterator -> 'val t
    
    val imap: ('val -> 'new) -> 'val iterator -> 'new iterator
    val uniq: ('val * 'val -> bool) -> 'val iterator -> 'val iterator
    val merge: ('v1 * 'v2 -> 'new) -> ('v1 iterator * 'v2 iterator) -> 'new iterator
  end

functor ZTree(Order : ORDER) : ZTREE 
  where type key = Order.t =
  struct
    open Order
    type key = Order.t
    
    datatype 'val t = 
      Leaf of 'val | 
      Node of 'val t * key * 'val t
    
    fun uniform v = Leaf v
    fun range (u, l, r, v) = Node (Node (Leaf u, l, Leaf v), r, Leaf u)
    
    fun size (Leaf v) = 1
      | size (Node (l, _, r)) = size l + size r
    
    fun equal eq (Leaf v1, Leaf v2) = eq (v1, v2)
      | equal eq (Node _, Leaf _) = false
      | equal eq (Leaf _, Node _) = false
      | equal eq (Node (l1, k1, r1), Node (l2, k2, r2)) =
          not (k1 < k2) andalso not (k2 < k1) andalso 
          equal eq (l1, l2) andalso equal eq (r1, r2)
    
    fun app f (Leaf v) = f v
      | app f (Node (l, k, r)) = (app f l; app f r)
    
    fun map f (Leaf v) = Leaf (f v)
      | map f (Node (l, k, r)) = Node (map f l, k, map f r)
    
    fun fold f a t =
      let
        fun deep (x, y, Leaf v, a) = f (x, v, y, a) 
          | deep (x, y, Node (l, k, r), a) =
              deep (SOME k, y, r, deep (x, SOME k, l, a))
      in
        deep (NONE, NONE, t, a)
      end
    
    fun foldr f a t =
      let
        fun deep (x, y, Leaf v, a) = f (x, v, y, a) 
          | deep (x, y, Node (l, k, r), a) =
              deep (x, SOME k, l, deep (SOME k, y, r, a))
      in
        deep (NONE, NONE, t, a)
      end
    
    fun lookup (Leaf v) _ = v
      | lookup (Node (l, k, r)) x = 
        if x < k then lookup l x else lookup r x
    
    datatype 'val iterator = 
      Iter of 'val * key option * (unit -> 'val iterator)
    
    fun front t =
      let
        datatype 'val stack = Parent of key option * 'val t
        fun goleft (Leaf v, c, stack) = Iter (v, c, next stack)
          | goleft (Node (l, k, r), c, stack) = 
              goleft (l, SOME k, Parent (c, r) :: stack)
        and next [] () = raise Subscript
          | next (Parent (c, r) :: stack) () = 
              goleft (r, c, stack)
      in
        goleft (t, NONE, [])
      end
    
    fun back t =
      let
        datatype 'val stack = Parent of key option * 'val t
        fun goright (Leaf v, c, stack) = Iter (v, c, next stack)
          | goright (Node (l, k, r), c, stack) = 
              goright (r, SOME k, Parent (c, l) :: stack)
        and next [] () = raise Subscript
          | next (Parent (c, l) :: stack) () = 
              goright (l, c, stack)
      in
        goright (t, NONE, [])
      end
    
    fun fromFront f =
      let
        fun suck (Iter (v1, NONE, iter), r) = (v1, NONE) :: r
          | suck (Iter (v1, k1, iter), r) = suck (iter (), (v1, k1) :: r)
        val table = Vector.fromList (suck (f, []))
        fun grow (l, r) =
          if l + 1 = r then Leaf (#1 (Vector.sub (table, l))) else
          let val m = (l+r) div 2 in
            Node (grow (m, r), 
                  valOf (#2 (Vector.sub (table, m))), 
                  grow (l, m)) 
          end
      in
        grow (0, Vector.length table)
      end
    
    fun imap f iter =
      let
        fun wrap (Iter (v, NONE, n)) () = Iter (f v, NONE, wrap (Iter (v, NONE, n)))
          | wrap (Iter (v, k, n)) () = Iter (f v, k, wrap (n ()))
      in
        wrap iter ()
      end
    
    fun uniq eq iter =
      let
        fun wrap (Iter (v, NONE, n)) () = Iter (v, NONE, n)
          | wrap (Iter (v1, k1, n1)) () =
              case n1 () of Iter (v2, k2, n2) =>
                if eq (v1, v2) then wrap (Iter (v2, k2, n2)) () else
                Iter (v1, k1, wrap (Iter (v2, k2, n2)))
      in
        wrap iter ()
      end
    
    fun merge f (iter1, iter2) =
      let
        fun wrap (Iter (v1, NONE, n1), Iter (v2, NONE, n2)) () = 
              Iter (f (v1, v2), NONE, wrap (Iter (v1, NONE, n1), Iter (v2, NONE, n2)))
          | wrap (Iter (v1, SOME k1, n1), Iter (v2, NONE, n2)) () =
              Iter (f (v1, v2), SOME k1, wrap (n1 (), Iter (v2, NONE, n2)))
          | wrap (Iter (v1, NONE, n1), Iter (v2, SOME k2, n2)) () = 
              Iter (f (v1, v2), SOME k2, wrap (Iter (v1, NONE, n1), n2 ()))
          | wrap (Iter (v1, SOME k1, n1), Iter (v2, SOME k2, n2)) () =
              if k1 < k2 then
                Iter (f (v1, v2), SOME k1, wrap (n1 (), Iter (v2, SOME k2, n2)))
              else if k2 < k1 then
                Iter (f (v1, v2), SOME k2, wrap (Iter (v1, SOME k1, n1), n2 ()))
              else
                Iter (f (v1, v2), SOME k1, wrap (n1 (), n2 ()))
      in
        wrap (iter1, iter2) ()
      end
  end
