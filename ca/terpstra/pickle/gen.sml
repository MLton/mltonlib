 structure Order = struct type k = string val order = String.compare end
structure Set = Set(Order)
structure Map = Map(Order)

infix 5 && ++
fun l && x = x :: l
fun l ++ f = f l

fun foldl  (f, v) l = Vector.foldl  (fn (x, l) => f x l) l v
fun foldli (f, v) l = Vector.foldli (fn (i, x, l) => f (i, x) l) l v
fun sfoldl (f, s, v) l =
  let
    fun sep (0, s) = ""
      | sep (_, s) = s
    fun gen (i, x, l) = l && sep (i, s) ++ f x
  in
    Vector.foldli gen l v
  end

fun tuple (f, v) l =
  if Vector.length v = 0 then l else
  l && "(" ++ sfoldl (fn x => fn l => l && f x, ", ", v) && ") "

local
  fun bindToKey { name, reader, writer, tyvars=_, typ=_ } =
    (name, { reader = reader, writer = writer })
in
  fun defd (bind:'a bind_typ vector) =
    Map.fromVector (Vector.map bindToKey bind)
end
