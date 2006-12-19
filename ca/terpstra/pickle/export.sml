fun export ((data, bind), (l, scope)) =
  let
    fun decl { name, reader=_, writer=_, tyvars, typ=_ } l =
      l && "    type " ++ tuple (#1, tyvars) && name && "\n"
    fun binder ({ name, reader=_, writer=_, tyvars=_, typ=_ }, m) =
      Map.insert m (name, { reader = "", writer = "" })
      
    val l = l ++ foldl (decl, bind)
    val scope = Vector.foldl binder scope bind
    val (l, scope) = dtype ((data, Vector.fromList []), (l, scope))
    
    fun typack rw (t, _) l = l && t && " Base." && rw
    fun tyfun rw v l =
      if Vector.length v = 0 then l else
      l ++ sfoldl (typack rw, " * ", v) && " -> " ++ tuple (#1, v)
    fun methods {name, reader=_, writer=_, tyvars, typ=_} l = 
      l && "    val " && name 
      && ": { r: " ++ tyfun "r" tyvars && name && " Base.r, w: "
      ++ tyfun "w" tyvars && name && " Base.w }\n"
  in
    (l ++ foldl (methods, data) ++ foldl (methods, bind), scope)
  end
