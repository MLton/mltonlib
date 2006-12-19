fun import ((data, bind) : toplevel_typ, (l, scope)) =
  let
    val withscope = Map.union (scope, defd data)
    val datascope = Map.union (withscope, defd bind)
    val imports = ref Set.empty
    
    fun refd (typ, map) =
      case typ of
          (RECORD v) => Vector.foldl refd map (Vector.map #2 v)
        | (TUPLE v) => Vector.foldl refd map v
        | (TYVAR _) => map
        | (RECURSIVE (n, v, _)) => 
            Vector.foldl refd
              (Map.insert map (n, Vector.length v)) 
              (Vector.map #1 v)
    
    fun prune scope map =
      let
        fun filter ((k, v), m) =
          case Map.fetch scope k of
              NONE => Map.insert m (k, v)
            | SOME _ => m
      in
        Map.foldl filter Map.empty map
      end
    
    val withtyp = Vector.map #typ bind
    val datatyp = Vector.map #typ data
    val withref = prune withscope (Vector.foldl refd Map.empty withtyp)
    fun constr ((_, NONE), m) = m
      | constr ((_, SOME typ), m) = refd (typ, m)
    fun datav ((_, _, v), m) = Vector.foldl constr m v
    val dataref = prune datascope (Vector.foldl datav Map.empty datatyp)
    
    val import = Map.union (dataref, withref)
    
    fun fakescope ((k, _), m) = Map.insert m (k, {reader = "", writer = ""})
    val outscope = Map.foldl fakescope datascope import
    
    fun tyvar 1 l = l && "'a1"
      | tyvar i l = l && "'a" && Int.toString i && ", " ++ tyvar (i - 1)
    fun tyvars n l =
      if n = 0 then l else
      l && "(" ++ tyvar n && ") "
    fun tyarg rw 1 l = l && "'a1 Base." && rw
      | tyarg rw i l = l && "'a" && Int.toString i && " Base." && rw && " * " 
                         ++ tyarg rw (i - 1)
    fun tyfun rw n l =
      if n = 0 then l else
      l ++ tyarg rw n && " -> " ++ tyvars n
    
    fun declare ((k, v), l) =
      l && "    type " ++ tyvars v && k && "\n"
      && "    val " && k && ": { r: " ++ tyfun "r" v && k 
      && " Base.r, w: " ++ tyfun "w" v && k && " Base.w }\n"
  in
    (Map.foldl declare l import, outscope)
  end
