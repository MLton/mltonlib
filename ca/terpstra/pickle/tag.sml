fun tag ast =
  let
    fun topdefd ((data, bind), m) =
      Map.union (Map.union (m, defd bind), defd data)
    fun toSet m = Map.foldl (fn ((k, _), s) => Set.insert s k) Set.empty m
    val ids = ref (toSet (List.foldl topdefd Map.empty ast))
    
    val c = ref 0
    fun pick pfx () = 
      let
        val id = pfx ^ Int.toString (!c)
        val () = c := (!c) + 1
      in
        if Set.member (!ids) id then pick pfx () else 
        id before ids := Set.insert (!ids) id
      end
    val pickId = pick "v_"
    val pickFn = pick "f_"
    
    fun tymap tyvars = Vector.map (fn (x, _) => (x, pickFn ())) tyvars
    fun findFn tyvars n = 
      let
        exception UnboundTyvar
        val x = Vector.find (fn (m, _) => n = m) tyvars
      in
        case x of
            NONE => raise UnboundTyvar
          | SOME (_, f) => f
      end
    
    fun inline tyvars typ =
      case typ of
          (RECORD v) => 
            RECORD (Vector.map (fn (n, t) => (n, inline tyvars t)) v)
        | (TUPLE v) => 
            TUPLE (Vector.map (inline tyvars) v)
        | (TYVAR (tyvar, _, _)) => 
            TYVAR (tyvar, pickId (), findFn tyvars tyvar)
        | (RECURSIVE (name, tyv, _)) => 
            RECURSIVE (name, Vector.map (maprec tyvars) tyv, pickId ())
    and maprec tyvars (ty, _) = (inline tyvars ty, pickFn ())
    
    fun bindtyp f { name, reader, writer, tyvars, typ } =
      let val tyvars = tymap tyvars
      in { name = name, reader = pickFn (), writer = pickFn (), 
           tyvars = tyvars, typ = f tyvars typ } end
    
    fun datatyp tyvars (_, _, v) =
      (pickFn (), pickFn (), Vector.map 
        (fn (name, inlineo) => (name, Option.map (inline tyvars) inlineo))
        v)
    
    fun process (data, bind) =
      (Vector.map (bindtyp datatyp) data, Vector.map (bindtyp inline) bind)
  in
    (pickFn (), List.map process ast)
  end
